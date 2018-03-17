#include "contiki.h"
#include "net/rime.h"
#include "powertrace.h"
#include "random.h"

#include "dev/button-sensor.h"
#include "dev/leds.h"

#include <stdio.h>

static void recv(struct unicast_conn *, const rimeaddr_t *);
static void sent(struct unicast_conn *, int, int);

static const struct unicast_callbacks callbacks = {recv, sent};
static struct unicast_conn conn;

PROCESS(task_3, "Task 3");
AUTOSTART_PROCESSES(&task_3);

PROCESS_THREAD(task_3, ev, data) {
  PROCESS_EXITHANDLER(unicast_close(&conn);)
  PROCESS_BEGIN();

  unicast_open(&conn, 146, &callbacks);

  while (1) {
    rimeaddr_t addr;
    addr.u8[0] = 1;
    addr.u8[1] = 0;

    static struct etimer et;
    etimer_set(&et, CLOCK_SECOND);

    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));

    packetbuf_copyfrom("Hello", 6);

    if (!rimeaddr_cmp(&addr, &rimeaddr_node_addr)) {
      unicast_send(&conn, &addr);
      printf("Unicast message sent\n");
    }
  }

  PROCESS_END();
}

static void recv(struct unicast_conn *c, const rimeaddr_t *from) {
  printf("Unicast message received from %d.%d\n", from->u8[0], from->u8[1]);
}

static void sent(struct unicast_conn *c, int status, int num_tx) {
  const rimeaddr_t *dest = packetbuf_addr(PACKETBUF_ADDR_RECEIVER);

  if (rimeaddr_cmp(dest, &rimeaddr_null)) {
    return;
  }

  printf("unicast message sent to %d.%d: status %d num_tx %d\n", dest->u8[0],
         dest->u8[1], status, num_tx);
}
