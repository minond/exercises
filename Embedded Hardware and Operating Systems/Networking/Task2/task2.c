/**
 * Simulate multihop (Rime) in Cooja. Receiver nodes must print out receiving
 * package i.e. your name. Note: reuse and modify an example code in
 * example-multihop.c file.
 */

#include "contiki.h"
#include "net/rime.h"
#include "random.h"

#include <stdio.h>

#define CHANNEL 135

static void recv(struct multihop_conn *, const rimeaddr_t *, const rimeaddr_t *,
                 uint8_t hops);
static rimeaddr_t *frwd(struct multihop_conn *, const rimeaddr_t *,
                        const rimeaddr_t *, const rimeaddr_t *, uint8_t hops);

static const struct multihop_callbacks callbacks = {recv, frwd};
static struct multihop_conn multihop;

PROCESS(task2, "Simulate multihop (Rime) in Cooja.");
AUTOSTART_PROCESSES(&task2);

PROCESS_THREAD(task2, ev, data) {
  static struct etimer timer;

  PROCESS_EXITHANDLER(multihop_close(&multihop);)
  PROCESS_BEGIN();

  multihop_open(&multihop, CHANNEL, &callbacks);

  while (1) {
    rimeaddr_t to;
    to.u8[0] = 1;
    to.u8[1] = 0;

    etimer_set(&timer, CLOCK_SECOND * 5 + random_rand());

    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    packetbuf_copyfrom("Marcos", 6);
    multihop_send(&multihop, &to);
  }

  PROCESS_END();
}

static void recv(struct multihop_conn *conn, const rimeaddr_t *from,
                 const rimeaddr_t *prev, uint8_t hops) {
  printf("Received multihop message from %d.%d through %d.%d: '%s'\n",
         from->u8[0], from->u8[1], prev->u8[0], prev->u8[1],
         (char *)packetbuf_dataptr());
}

static rimeaddr_t *frwd(struct multihop_conn *conn, const rimeaddr_t *orig,
                        const rimeaddr_t *dest, const rimeaddr_t *prev,
                        uint8_t hops) {
  printf("Received multihop message %d.%d -> %d.%d -> %d.%d: '%s'\n",
         orig->u8[0], orig->u8[1], prev->u8[0], prev->u8[1], dest->u8[0],
         dest->u8[1], (char *)packetbuf_dataptr());

  return NULL;
}
