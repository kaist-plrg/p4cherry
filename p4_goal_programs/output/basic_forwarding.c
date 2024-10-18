// #include <cluj.h>
// #include <endian.h>
// #include <stdbool.h>
// #include <stdint.h>
#include <stdlib.h>
// #include <unistd.h>
#include <cluj_core.h>

const uint16_t TYPE_IPV4 = 0x800;

typedef struct ethernet_t {
  uint64_t dstAddr:48;
  uint64_t srcAddr:48;
  uint16_t etherType:16;
  char isValid;
} ethernet_t;

typedef struct ipv4_t {
  uint8_t  version:4;
  uint8_t  ihl:4;
  uint8_t  diffserv;
  uint16_t totalLen;
  uint16_t identification;
  uint8_t  flags:3;
  uint16_t fragOffset:13;
  uint8_t  ttl;
  uint8_t  protocol;
  uint16_t hdrChecksum;
  uint32_t srcAddr;
  uint32_t dstAddr;
  char isValid;
} ipv4_t;

typedef struct metadata {} metadata;

typedef struct headers {
  ethernet_t ethernet;
  ipv4_t ipv4;
} headers;

void MyParser(headers* hdr, metadata* meta, standard_metadata_t* standard_metadata) {
    void* header = malloc(get_header_len());
    get_packet(header);
    goto start;

accept:
    return;

reject:
    return;
    
start:
hdr->ethernet.isValid = 0;
    hdr->ipv4.isValid = 0;
    goto parse_ethernet;

parse_ethernet:
    hdr->ethernet = *((ethernet_t*) header);
    hdr->ethernet.isValid = 1;
    header += sizeof(ethernet_t);
    switch (hdr->ethernet.etherType) {
        case TYPE_IPV4:
            goto parse_ipv4;
        default:
            goto accept;
    }
    
parse_ipv4:
    hdr->ipv4 = *((ipv4_t*) header);
    hdr->ipv4.isValid = 1;
    header += sizeof(ipv4_t);
    goto accept;
}

void MyVerifyChecksum(headers* hdr, metadata* meta) {
    return;
}

void MyIngress_action_drop(headers* hdr, metadata* meta, standard_metadata_t* standard_metadata) {
    mark_to_drop(standard_metadata);
}

void MyIngress_action_ipv4_forward(headers* hdr, metadata* meta, standard_metadata_t* standard_metadata, uint64_t dstAddr, uint16_t port) {
    standard_metadata->egress_spec = port;
    hdr->ethernet.srcAddr = hdr->ethernet.dstAddr;
    hdr->ethernet.dstAddr = dstAddr;
    hdr->ipv4.ttl = hdr->ipv4.ttl - 1;
}

// TODO: table ipv4_lpm

void MyIngress(headers* hdr, metadata* meta, standard_metadata_t* standard_metadata) {
    if (hdr->ipv4.isValid) {
        // TODO - ipv4_lpm.apply();
    }
}

void MyEgress(headers* hdr, metadata* meta, standard_metadata_t* standard_metadata) {
    return;
}

void MyUpdateChecksum(headers* hdr, metadata* meta) {
    // TODO: ipv4_lpm.update_checksum();
    return;
}

void MyDeparser(headers* hdr) {
    stage_packet(&hdr->ethernet);
    stage_packet(&hdr->ipv4);
    send_packet();
}

int main() {
    headers hdr;
    metadata meta;
    standard_metadata_t standard_metadata;
    MyParser(&hdr, &meta, &standard_metadata);
    MyVerifyChecksum(&hdr, &meta);
    MyIngress(&hdr, &meta, &standard_metadata);
    MyEgress(&hdr, &meta, &standard_metadata);
    MyUpdateChecksum(&hdr, &meta);
    MyDeparser(&hdr);
    return 0;
}

