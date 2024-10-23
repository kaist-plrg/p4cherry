#include "cluj_core.h"
#include <cluj.h>
#include <stdbool.h>

struct metadata { bool ok; };

struct headers { };

void MyParser (struct packet_t* packet_in, struct headers* hdr, struct metadata* meta, struct standard_metadata_t* standard_metadata) {
  start:
    goto accept;

  accept:
  reject:
    return;
}
void MyVerifyChecksum (struct headers* hdr, struct metadata* meta) { }
void MyIngress (struct headers* hdr, struct metadata* meta, struct standard_metadata_t* standard_metadata)
{
  (*standard_metadata).egress_spec = 1;
}
void MyEgress (struct headers* hdr,struct metadata* meta, struct standard_metadata_t* standard_metadata)
{ }
void MyUpdateChecksum (struct headers* hdr, struct metadata* meta) { }
void MyDeparser (struct* packet_out, struct headers* hdr) { }
int main () {
  struct packet_t packet_in, packet_out;
  init_packet_in(&packet_in);
  init_packet_in(&packet_out);
  struct headers h;
  struct metadata m;
  struct standard_metadata_t sm;
  MyParser(pkt_address, &hdr, &m, &sm);
  MyVerifyChecksum(&h, &m);
  MyIngress(&h, &m, &sm);
  MyEgress(&h, &m, &sm);
  MyUpdateChecksum(&h, &m);
  MyDeparser(&h);
}