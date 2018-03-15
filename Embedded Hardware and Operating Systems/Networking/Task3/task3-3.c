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

static void recv(struct trickle_conn *);

const static struct trickle_callbacks callbacks = {recv};
static struct trickle_conn trickle;

PROCESS(task3_3, "Simulate trickle (Rime) in Cooja (1)");
AUTOSTART_PROCESSES(&task3_3);

PROCESS_THREAD(task3_3, ev, data) {
  PROCESS_EXITHANDLER(trickle_close(&trickle);)
  PROCESS_BEGIN();

  trickle_open(&trickle, 10*CLOCK_SECOND, 145, &callbacks);
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
}
