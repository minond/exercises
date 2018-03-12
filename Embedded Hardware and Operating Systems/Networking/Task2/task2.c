/**
 * Simulate multihop (Rime) in Cooja. Receiver nodes must print out receiving
 * package i.e. your name. Note: reuse and modify an example code in
 * example-multihop.c file.
 */

#include "contiki.h"
#include "lib/list.h"
#include "lib/memb.h"
#include "net/rime.h"
#include "random.h"
#include "sys/node-id.h"

#include <stdio.h>

#define CHANNEL 135
#define MSG_LEN 36
#define MAX_NEIGHBORS 15

struct neighbor_t {
  rimeaddr_t addr;
};

static void save(struct announcement *, const rimeaddr_t *, uint16_t, uint16_t);

static void recv(struct multihop_conn *, const rimeaddr_t *, const rimeaddr_t *,
                 uint8_t hops);

static rimeaddr_t *frwd(struct multihop_conn *, const rimeaddr_t *,
                        const rimeaddr_t *, const rimeaddr_t *, uint8_t hops);

static struct announcement ann;
static struct multihop_conn multihop;
static const struct multihop_callbacks callbacks = {recv, frwd};

LIST(neighbors);
MEMB(neighbor_mem, struct neighbor_t, MAX_NEIGHBORS);

PROCESS(task2, "Simulate multihop (Rime) in Cooja.");
AUTOSTART_PROCESSES(&task2);

PROCESS_THREAD(task2, ev, data) {
  char buff[MSG_LEN];
  static struct etimer timer;
  rimeaddr_t to = {{1, 0}};

  PROCESS_EXITHANDLER(multihop_close(&multihop);)
  PROCESS_BEGIN();

  multihop_open(&multihop, CHANNEL, &callbacks);
  announcement_register(&ann, CHANNEL, save);
  announcement_set_value(&ann, 0);

  while (1) {
    etimer_set(&timer, CLOCK_SECOND * 5 + random_rand());

    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    sprintf(buff, "NodeId[%d]", node_id);
    packetbuf_copyfrom(buff, MSG_LEN);
    multihop_send(&multihop, &to);
  }

  PROCESS_END();
}

static void save(struct announcement *ann, const rimeaddr_t *from, uint16_t id,
                 uint16_t val) {
  printf("Received announcement from %d.%d\n", from->u8[0], from->u8[1]);

  struct neighbor_t *neighbor;
  neighbor = memb_alloc(&neighbor_mem);

  if (neighbor != NULL) {
    rimeaddr_copy(&neighbor->addr, from);
    list_add(neighbors, neighbor);
    printf("Saved neighbor %d.%d\n", from->u8[0], from->u8[1]);
  } else {
    printf("Error saving neighbor %d.%d\n", from->u8[0], from->u8[1]);
  }
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

  struct neighbor_t *neighbor;
  int len = list_length(neighbors);
  int nidx, i;

  if (len > 0) {
    i = 0;
    nidx = random_rand() % len;
    neighbor = list_head(neighbors);

    for (; neighbor != NULL && i != nidx; ++i) {
      neighbor = list_item_next(neighbor);
    }

    if (neighbor != NULL) {
      printf("Forwarding message to %d.%d (hop #%d)\n", neighbor->addr.u8[0],
             neighbor->addr.u8[1], packetbuf_attr(PACKETBUF_ATTR_HOPS));

      return &neighbor->addr;
    } else {
      printf("Failed to get neighbor from list, not forwarding message\n");
    }
  } else {
    printf("No neighbors, not forwarding message\n");
  }

  return NULL;
}
