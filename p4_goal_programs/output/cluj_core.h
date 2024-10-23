#include <assert.h>

struct standard_metadata_t {
  unsigned int ingress_port: 9;
  unsigned int egress_spec: 9;
  unsigned int egress_port: 9;
  unsigned int instance_type: 32;
  unsigned int packet_length: 32;
  unsigned int enq_timestamp: 32;
  unsigned int enq_qdepth: 19;
  unsigned int deq_timedelta: 32;
  unsigned int deq_qdepth: 19;
  unsigned long int ingress_global_timestamp: 48;
  unsigned long int egress_global_timestamp: 48;
  unsigned int mcast_grp: 16;
  unsigned int egress_rid: 16;
  unsigned int checksum_error: 1;
  char parser_error;
  unsigned int priority: 3;
} standard_metadata_t;

struct packet_in {
  uint8_t* buffer; // Pointer to the start of the raw packet buffer
  size_t length; // Length of the packet data
  size_t cursor = 0; // Current position read in the packet
};

struct packet_out {
  uint8_t* buffer; // Pointer to the start of the raw packet buffer
  size_t cursor = 0; // Current position read in the packet
};

/* Given an uninitialized packet, it initializes the packet_t->buffer to the memory location of the start of the input packet data in LUSS LMEM.
Eg. {.buffer = (LMEM_PARCEL_BASE + 12) << 3; length = [DispatchCookie.pkt_head_len]}
*/
void init_packet_in(packet_in* packet);

/* Given an uninitialized packet, it initializes the packet_t->buffer to the memory location of the start of some unused memory for the output packet data to be populated in LUSS LMEM.
*/
void init_packet_out(packet_out* packet);

void memcpy(void* destination, void* source, size_t length);

void extract(packet_in* packet, void* destination, size_t length) {
  memcpy(destination, packet->buffer + packet->cursor, length);
  packet->cursor += length;
}

void advance(packet_in* packet, size_t length) {
  packet->cursor += length;
}

// void lookahead(packet_in* packet, void* destination, size_t length)

void emit(packet_out* packet, void* source, size_t length) {
  memcpy(packet->buffer + packet->cursor, source, length);
  packet->cursor += length;
}