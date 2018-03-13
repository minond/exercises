/**
 * Simulate trickle (Rime) in Cooja. Modify an etimer value in the code with
 * different values:
 *
 *   - 10 power of (-20) * CLOCK_SECOND,
 *   - 10 power of (-14) * CLOCK_SECOND, and
 *   - 10 power of (-6) * CLOCK_SECOND
 *
 * Reuse and modify an example code in example-trickle.c file.
 */

#include "contiki.h"
#include "dev/button-sensor.h"
#include "net/rime/trickle.h"

#include <stdio.h>

static short pows(short, short);
static void recv(struct trickle_conn *);

const static struct trickle_callbacks callbacks = {recv};
static struct trickle_conn trickle;

PROCESS(task3_3, "Simulate trickle (Rime) in Cooja (1)");
AUTOSTART_PROCESSES(&task3_3);

PROCESS_THREAD(task3_3, ev, data) {
  short interval = pows(10, -6 * CLOCK_SECOND);

  PROCESS_EXITHANDLER(trickle_close(&trickle);)
  PROCESS_BEGIN();

  printf("Trickle every %i seconds\n", interval);
  trickle_open(&trickle, CLOCK_SECOND, interval, &callbacks);
  SENSORS_ACTIVATE(button_sensor);

  while (1) {
    PROCESS_WAIT_EVENT_UNTIL(ev == sensors_event && data == &button_sensor);

    packetbuf_copyfrom("Hello, world", 13);
    trickle_send(&trickle);
  }

  PROCESS_END();
}

static void recv(struct trickle_conn *conn) {
  printf("Trickle message received '%s'\n", (char *)packetbuf_dataptr());

static short pows(short n, short x) {
  if (x < 0) {
    return pows(1 / x, -x);
  } else if (x == 0) {
    return 1;
  } else if (x == 1) {
    return n;
  } else if (x % 2 == 0) {
    return pows(n * n, x / 2);
  } else {
    return n * pows(n * n, (x - 1) / 2);
  }
}
}
