/**
 * Configure the RDC driver into Null_RDC, X-MAC and LPP. Measure the
 * energy consumption for each conf iguration. Which RDC consumes the
 * highest energy and which one is the lowest?
 */

#include "contiki.h"
#include "net/rime.h"
#include "powertrace.h"
#include "random.h"

#include "dev/button-sensor.h"
#include "dev/leds.h"

#include <stdio.h>

static void recv(struct broadcast_conn *, const rimeaddr_t *);

static const struct broadcast_callbacks callbacks = {recv};
static struct broadcast_conn broadcast;

PROCESS(lpp_power, "LPP Power Usage");
AUTOSTART_PROCESSES(&lpp_power);

PROCESS_THREAD(lpp_power, ev, data) {
  static struct etimer et;
  unsigned long last_cpu, last_lpm, last_tra, last_lis, curr_cpu, curr_lpm,
      curr_tra, curr_lis;

  last_cpu = 0;
  last_lpm = 0;
  last_tra = 0;
  last_lis = 0;

  PROCESS_EXITHANDLER(broadcast_close(&broadcast);)
  PROCESS_BEGIN();

  powertrace_start(CLOCK_SECOND * 2);
  broadcast_open(&broadcast, 129, &callbacks);

  while (1) {
    etimer_set(&et, CLOCK_SECOND * 10 + random_rand() % (CLOCK_SECOND * 4));

    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));

    packetbuf_copyfrom("Hello", 6);
    broadcast_send(&broadcast);
    printf("Sent broadcast message\n");

    curr_cpu = energest_type_time(ENERGEST_TYPE_CPU);
    curr_lpm = energest_type_time(ENERGEST_TYPE_LPM);
    curr_tra = energest_type_time(ENERGEST_TYPE_TRANSMIT);
    curr_lis = energest_type_time(ENERGEST_TYPE_LISTEN);

    printf("Usage: cpu %lu, lpm %lu, transmit %lu, listen %lu\n", curr_cpu,
           curr_lpm, curr_tra, curr_lis);

    printf("Deltas: cpu %lu, lpm %lu, transmit %lu, listen %lu\n",
           curr_cpu - last_cpu, curr_lpm - last_lpm, curr_tra - last_tra,
           curr_lis - last_lis);

    last_cpu = curr_cpu;
    last_lpm = curr_lpm;
    last_tra = curr_tra;
    last_lis = curr_lis;
  }

  PROCESS_END();
}

static void recv(struct broadcast_conn *c, const rimeaddr_t *from) {
  printf("Received broadcast message from %d.%d: '%s'\n", from->u8[0],
         from->u8[1], (char *)packetbuf_dataptr());
}
