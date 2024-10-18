#include "cluj_core.h"
#include <cluj.h>
#include <endian.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

struct metadata { char ok; } metadata;

struct headers { } headers;

void MyParser (uint16_t packet_addr, struct headers* hdr, struct metadata* meta, struct standard_metadata_t* standard_metadata) {
  start:
    goto accept;

  accept:
  reject:
    return;
}
void MyVerifyChecksum (struct headers* hdr, struct metadata* meta) { }
void MyIngress (struct headers* hdr, struct metadata* meta, struct standard_metadata_t* standard_metadata)
{
  (*standard_metadata).egress_spec = 2;
}
void MyEgress (struct headers* hdr,struct metadata* meta, struct standard_metadata_t* standard_metadata)
{ }
void MyUpdateChecksum (struct headers* hdr, struct metadata* meta) { }
void MyDeparser (struct headers* hdr) { }
int main () {
  uint16_t pkt_address = get_packet_addr();
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