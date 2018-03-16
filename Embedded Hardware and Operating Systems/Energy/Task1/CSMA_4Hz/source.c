/**
 * Create your own folder under /home/user/contiki­2.7/examples/ directory.
 * Copy /home/user/contiki 2.7/examples/powertrace and save it in your folder.
 * Configure the channel check rate to be 4, 8 and 16 Hz with ContikiMAC and
 * CSMA configuration. Use the energest module and measure the energy
 * dissipation for the three rates.
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

PROCESS(cmac_4hz, "Powertrace CSMA 4Hz Channel Check Rate");
AUTOSTART_PROCESSES(&cmac_4hz);

PROCESS_THREAD(cmac_4hz, ev, data) {
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
