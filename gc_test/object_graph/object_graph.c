// Test case that builds an object graph similar to the one used in tmux.  The
// graph consists of sessions and windows; each session can have several
// windows, and each window can be associated with several sessions.  A session
// with no windows or a window with no sessions will be implicitly destroyed.

#include <stdlib.h>
#include <stdio.h>
#include <sys/queue.h>

// If set, also define globals lists of all windows and all sessions.
//#define GLOBAL_LISTS

struct session;
struct window;

struct link {
    struct session* session;
    struct window* window;
    // An entry in the session's list of links.
    TAILQ_ENTRY(link) session_entry;
    // An entry in the window's list of links.
    TAILQ_ENTRY(link) window_entry;
};
TAILQ_HEAD(links, link);

struct session {
    int id;
    TAILQ_ENTRY(session) global_entry;
    struct links links;
};
#ifdef GLOBAL_LISTS
TAILQ_HEAD(sessions, session);
static struct sessions global_sessions = TAILQ_HEAD_INITIALIZER(global_sessions);
#endif // GLOBAL_LISTS
static int next_session_id = 0;

struct window {
    int id;
    TAILQ_ENTRY(window) global_entry;
    struct links links;
};
#ifdef GLOBAL_LISTS
TAILQ_HEAD(windows, window);
static struct windows global_windows = TAILQ_HEAD_INITIALIZER(global_windows);
#endif // GLOBAL_LISTS
static int next_window_id = 0;



struct session* session_new();
void session_delete(struct session* sess);
void session_render(struct session* sess);

struct window* window_new(struct session* sess);
void window_delete(struct window* win);

void window_link(struct window* win, struct session* sess);
void window_unlink(struct window* win, struct session* sess);
#define CLEANUP_SESSION 1
#define CLEANUP_WINDOW 2
void link_delete(struct link* l, int cleanup_flags);


struct session* session_new() {
    struct session* sess = calloc(1, sizeof(struct session));
    sess->id = ++next_session_id;
    printf("new session %d\n", sess->id);
    TAILQ_INIT(&sess->links);
#ifdef GLOBAL_LISTS
    TAILQ_INSERT_TAIL(&global_sessions, sess, global_entry);
#endif

    // Create an initial window for the session.
    (void)window_new(sess);

    return sess;
}

void session_delete(struct session* sess) {
    printf("delete session %d\n", sess->id);
    // Unlink all windows
    struct link* l = TAILQ_FIRST(&sess->links);
    while (l != NULL) {
        struct link* next = TAILQ_NEXT(l, session_entry);
        link_delete(l, CLEANUP_WINDOW);
        l = next;
    }

    // Destroy the session
#ifdef GLOBAL_LISTS
    TAILQ_REMOVE(&global_sessions, sess, global_entry);
#endif
    printf("deleted session %d\n", sess->id);
    free(sess);
}

void session_render(struct session* sess) {
    printf("session %d windows: ", sess->id);
    struct link* l;
    int first = 1;
    TAILQ_FOREACH(l, &sess->links, session_entry) {
        if (first) {
            first = 0;
            printf("%d", l->window->id);
        } else {
            printf(", %d", l->window->id);
        }
    }
    if (first) {
        // No links were found - should be impossible
        printf("(none)\n");
    } else {
        printf("\n");
    }
}


struct window* window_new(struct session* sess) {
    struct window* win = calloc(1, sizeof(struct window));
    win->id = ++next_window_id;
    printf("new window %d\n", win->id);
    TAILQ_INIT(&win->links);
#ifdef GLOBAL_LISTS
    TAILQ_INSERT_TAIL(&global_windows, win, global_entry);
#endif

    window_link(win, sess);

    return win;
}

void window_delete(struct window* win) {
    printf("delete window %d\n", win->id);

    // Unlink all sessions
    struct link* l = TAILQ_FIRST(&win->links);
    while (l != NULL) {
        struct link* next = TAILQ_NEXT(l, window_entry);
        link_delete(l, CLEANUP_SESSION);
        l = next;
    }

    // Destroy the window
#ifdef GLOBAL_LISTS
    TAILQ_REMOVE(&global_windows, win, global_entry);
#endif
    printf("deleted window %d\n", win->id);
    free(win);
}

void window_link(struct window* win, struct session* sess) {
    printf("link session %d, window %d\n", sess->id, win->id);
    struct link* link = calloc(1, sizeof(struct link));
    link->session = sess;
    link->window = win;
    TAILQ_INSERT_TAIL(&sess->links, link, session_entry);
    TAILQ_INSERT_TAIL(&win->links, link, window_entry);
}

void window_unlink(struct window* win, struct session* sess) {
    struct link* l;
    TAILQ_FOREACH(l, &win->links, window_entry) {
        if (l->session == sess) {
            break;
        }
    }
    if (l != NULL) {
        link_delete(l, CLEANUP_SESSION | CLEANUP_WINDOW);
    }
}

void link_delete(struct link* l, int cleanup_flags) {
    printf("unlink session %d, window %d\n", l->session->id, l->window->id);

    if (cleanup_flags & CLEANUP_SESSION) {
        TAILQ_REMOVE(&l->session->links, l, session_entry);
        if (TAILQ_EMPTY(&l->session->links)) {
            session_delete(l->session);
        }
    }

    if (cleanup_flags & CLEANUP_WINDOW) {
        TAILQ_REMOVE(&l->window->links, l, window_entry);
        if (TAILQ_EMPTY(&l->window->links)) {
            window_delete(l->window);
        }
    }

    free(l);
}


int main() {
    struct session* sess1 = session_new();
    struct session* sess2 = session_new();

    printf("\ndelete a window explicitly, which removes it from its sessions\n");
    struct window* win3 = window_new(sess1);
    window_link(win3, sess2);

    session_render(sess1);
    session_render(sess2);

    window_delete(win3);

    session_render(sess1);
    session_render(sess2);

    printf("\ndelete a window implicitly by removing it from all sessions\n");
    struct window* win4 = window_new(sess1);
    window_unlink(win4, sess1);

    session_render(sess1);
    session_render(sess2);

    printf("\ndelete a session implicitly by removing all of its windows\n");
    struct session* sess3 = session_new();
    struct window* win5 = TAILQ_FIRST(&sess3->links)->window;
    window_unlink(win5, sess3);

    printf("\ndelete a session implicitly by deleting all of its windows\n");
    struct session* sess4 = session_new();
    struct window* win6 = TAILQ_FIRST(&sess4->links)->window;
    window_delete(win6);

    printf("\ndelete sessions, which removes and deletes all of their windows\n");
    struct window* win7 = window_new(sess1);
    window_link(win7, sess2);

    session_render(sess1);
    session_render(sess2);

    session_delete(sess2);
    session_delete(sess1);
}

