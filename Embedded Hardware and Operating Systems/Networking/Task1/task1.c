/**
 * Simulate broadcast (Rime) in Cooja. A random node broadcasts to all nodes
 * when both an etimer event and a button sensor event are triggered. Tips:
 * reuse and modify an example code in example-broadcast.c file. The file
 * example-trickle.c can be used as a reference for using buttons.
 */

#include "contiki.h"
#include "dev/button-sensor.h"
#include "net/rime.h"
#include "random.h"

#include <stdio.h>

PROCESS(task1, "Simulate broadcast (Rime) in Cooja.");
AUTOSTART_PROCESSES(&task1);

static void broadcast_recv(struct broadcast_conn *, const rimeaddr_t *);
static struct broadcast_conn broadcast;
static const struct broadcast_callbacks broadcast_call = {broadcast_recv};

PROCESS_THREAD(task1, ev, data) {
  static struct etimer timer;

  PROCESS_EXITHANDLER(broadcast_close(&broadcast);)
  PROCESS_BEGIN();

  broadcast_open(&broadcast, 129, &broadcast_call);
  SENSORS_ACTIVATE(button_sensor);

  while (1) {
    etimer_set(&timer, CLOCK_SECOND * 4 + random_rand() % (CLOCK_SECOND * 4));

    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer) ||
                             (ev == sensors_event && data == &button_sensor));

    if (ev == sensors_event) {
      packetbuf_copyfrom("evbtn", 5);
    } else {
      packetbuf_copyfrom("evtimer", 7);
    }

    broadcast_send(&broadcast);
    printf("Sent broadcast message\n");
  }

  PROCESS_END();
}

static void broadcast_recv(struct broadcast_conn *conn,
                           const rimeaddr_t *from) {
  printf("Received broadcast message from %d.%d: '%s'\n", from->u8[0],
         from->u8[1], (char *)packetbuf_dataptr());
}
