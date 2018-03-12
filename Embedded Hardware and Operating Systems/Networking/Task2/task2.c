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
#define NEIGHBOR_TTL 60 * CLOCK_SECOND
#define MAX_NEIGHBORS 15

struct neighbor_t {
  rimeaddr_t addr;
  struct ctimer ttl;
};

static rimeaddr_t *get_rand_addr();
static void drop(void *);
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
  rimeaddr_t *addr;
  static struct etimer timer;

  PROCESS_EXITHANDLER(multihop_close(&multihop);)
  PROCESS_BEGIN();

  multihop_open(&multihop, CHANNEL, &callbacks);
  announcement_register(&ann, CHANNEL, save);
  announcement_set_value(&ann, 0);

  while (1) {
    etimer_set(&timer, (CLOCK_SECOND * 5 + random_rand()) % 30);

    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    addr = get_rand_addr();

    if (addr != NULL) {
      printf("Sending a message to %d.%d\n", addr->u8[0], addr->u8[1]);
      sprintf(buff, "NodeId[%d]", node_id);
      packetbuf_copyfrom(buff, MSG_LEN);
      multihop_send(&multihop, addr);
    } else {
      printf("Unable to get neighbor address, not sending message\n");
    }
  }

  PROCESS_END();
}

static void drop(void *obj) {
  struct neighbor_t *neighbor = obj;

  printf("Removing neighbor %d.%d\n", neighbor->addr.u8[0],
         neighbor->addr.u8[1]);
  list_remove(neighbors, neighbor);
  memb_free(&neighbor_mem, neighbor);
}

static void save(struct announcement *ann, const rimeaddr_t *from, uint16_t id,
                 uint16_t val) {
  printf("Received announcement from %d.%d\n", from->u8[0], from->u8[1]);

  struct neighbor_t *neighbor;
  neighbor = memb_alloc(&neighbor_mem);

  if (neighbor != NULL) {
    rimeaddr_copy(&neighbor->addr, from);
    ctimer_set(&neighbor->ttl, NEIGHBOR_TTL, drop, neighbor);
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
  rimeaddr_t *addr = get_rand_addr();

  printf("Received multihop message %d.%d -> %d.%d -> %d.%d: '%s'\n",
         orig->u8[0], orig->u8[1], prev->u8[0], prev->u8[1], dest->u8[0],
         dest->u8[1], (char *)packetbuf_dataptr());

  if (addr != NULL) {
    printf("Forwarding message to %d.%d (hop #%d)\n", addr->u8[0], addr->u8[1],
           packetbuf_attr(PACKETBUF_ATTR_HOPS));
  } else {
    printf("Unable to get a neighbor address, not forwarding message\n");
  }

  return addr;
}

static rimeaddr_t *get_rand_addr() {
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
      return &neighbor->addr;
    }
  }

  return NULL;
}
