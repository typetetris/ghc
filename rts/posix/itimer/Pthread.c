/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2007
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * We use a realtime timer by default.  I found this much more
 * reliable than a CPU timer:
 *
 * Experiments with different frequences: using
 * CLOCK_REALTIME/CLOCK_MONOTONIC on Linux 2.6.32,
 *     1000us has  <1% impact on runtime
 *      100us has  ~2% impact on runtime
 *       10us has ~40% impact on runtime
 *
 * using CLOCK_PROCESS_CPUTIME_ID on Linux 2.6.32,
 *     I cannot get it to tick faster than 10ms (10000us)
 *     which isn't great for profiling.
 *
 * In the threaded RTS, we can't tick in CPU time because the thread
 * which has the virtual timer might be idle, so the tick would never
 * fire.  Therfore we used to tick in realtime in the threaded RTS and
 * in CPU time otherwise, but now we always tick in realtime, for
 * several reasons:
 *
 *   - resolution (see above)
 *   - consistency (-threaded is the same as normal)
 *   - more consistency: Windows only has a realtime timer
 *
 * Note we want to use CLOCK_MONOTONIC rather than CLOCK_REALTIME,
 * because the latter may jump around (NTP adjustments, leap seconds
 * etc.).
 */

#include "PosixSource.h"
#include "Rts.h"

#include "Ticker.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "posix/Clock.h"

/* As recommended in the autoconf manual */
# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#include <string.h>

#include <pthread.h>
#include <unistd.h>
#include <fcntl.h>

#if HAVE_SYS_TIMERFD_H
#include <sys/timerfd.h>
#define USE_TIMERFD_FOR_ITIMER 1
#else
#define USE_TIMERFD_FOR_ITIMER 0
#endif

/*
 * TFD_CLOEXEC has been added in Linux 2.6.26.
 * If it is not available, we use fcntl(F_SETFD).
 */
#ifndef TFD_CLOEXEC
#define TFD_CLOEXEC 0
#endif

static Time itimer_interval = DEFAULT_TICK_INTERVAL;

// Should we be firing ticks?
// Writers to this must hold the mutex below.
static volatile HsBool stopped = 0;

// should the ticker thread exit?
// This can be set without holding the mutex.
static volatile HsBool exited = 1;

// Signaled when we want to (re)start the timer
static pthread_cond_t start_cond;
static pthread_mutex_t mutex;
static pthread_t thread;

static void *itimer_thread_func(void *_handle_tick)
{
    TickProc handle_tick = _handle_tick;
    uint64_t nticks;
    int timerfd = -1;

#if USE_TIMERFD_FOR_ITIMER
    struct itimerspec it;
    it.it_value.tv_sec  = TimeToSeconds(itimer_interval);
    it.it_value.tv_nsec = TimeToNS(itimer_interval) % 1000000000;
    it.it_interval = it.it_value;

    timerfd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (timerfd == -1) {
        sysErrorBelch("timerfd_create");
        stg_exit(EXIT_FAILURE);
    }
    if (!TFD_CLOEXEC) {
      fcntl(timerfd, F_SETFD, FD_CLOEXEC);
    }
    if (timerfd_settime(timerfd, 0, &it, NULL)) {
        sysErrorBelch("timerfd_settime");
        stg_exit(EXIT_FAILURE);
    }
#endif

    while (!exited) {
        if (USE_TIMERFD_FOR_ITIMER) {
            if (read(timerfd, &nticks, sizeof(nticks)) != sizeof(nticks)) {
                if (errno != EINTR) {
                    sysErrorBelch("Itimer: read(timerfd) failed");
                }
            }
        } else {
            if (usleep(TimeToUS(itimer_interval)) != 0 && errno != EINTR) {
                sysErrorBelch("usleep(TimeToUS(itimer_interval) failed");
            }
        }

        // first try a cheap test
        if (stopped) {
            pthread_mutex_lock(&mutex);
            // should we really stop?
            if (stopped) {
                pthread_cond_wait(&start_cond, &mutex);
            }
            pthread_mutex_unlock(&mutex);
        } else {
            handle_tick(0);
        }
    }

    if (USE_TIMERFD_FOR_ITIMER)
        close(timerfd);
    pthread_mutex_destroy(&mutex);
    pthread_cond_destroy(&start_cond);
    return NULL;
}

void
initTicker (Time interval, TickProc handle_tick)
{
    itimer_interval = interval;
    stopped = 0;
    exited = 0;

    if (pthread_cond_init(&start_cond, NULL)) {
        sysErrorBelch("Itimer: Failed to initialize condition variable");
        stg_exit(EXIT_FAILURE);
    }

    if (pthread_mutex_init(&mutex, NULL)) {
        sysErrorBelch("Itimer: Failed to initialize mutex");
        stg_exit(EXIT_FAILURE);
    }

    if (! pthread_create(&thread, NULL, itimer_thread_func, (void*)handle_tick)) {
#if HAVE_PTHREAD_SETNAME_NP
        pthread_setname_np(thread, "ghc_ticker");
#endif
    } else {
        sysErrorBelch("Itimer: Failed to spawn thread");
        stg_exit(EXIT_FAILURE);
    }
}

void
startTicker(void)
{
    pthread_mutex_lock(&mutex);
    stopped = 0;
    pthread_cond_signal(&start_cond);
    pthread_mutex_unlock(&mutex);
}

/* There may be at most one additional tick fired after a call to this */
void
stopTicker(void)
{
    pthread_mutex_lock(&mutex);
    stopped = 1;
    pthread_mutex_unlock(&mutex);
}

/* There may be at most one additional tick fired after a call to this */
void
exitTicker (rtsBool wait)
{
    exited = 1;
    // ensure that ticker wakes up if stopped
    startTicker();

    // wait for ticker to terminate if necessary
    if (wait) {
        if (pthread_join(thread, NULL)) {
            sysErrorBelch("Itimer: Failed to join");
        }
    } else {
        pthread_detach(thread);
    }
}

int
rtsTimerSignal(void)
{
    return SIGALRM;
}
