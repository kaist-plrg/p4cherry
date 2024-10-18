typedef struct standard_metadata_t {
  unsigned int ingress_port;
  unsigned int egress_spec;
  unsigned int egress_port;
  unsigned int instance_type;
  unsigned int packet_length;
  unsigned int enq_timestamp;
  unsigned int enq_qdepth;
  unsigned int deq_timedelta;
  unsigned int deq_qdepth;
  unsigned long int ingress_global_timestamp;
  unsigned long int egress_global_timestamp;
  unsigned int mcast_grp;
  unsigned int egress_rid;
  unsigned int checksum_error;
  char parser_error;
  unsigned int priority;
} standard_metadata_t;

// Modifies the appropriate control register in Trio to drop the packet
extern void mark_to_drop(standard_metadata_t* standard_metadata);

// Gets the location of the packet in LMEM which is currently (LMEM_PARCEL_BASE + 12) << 3
extern uint16_t get_packet_addr();

// parcel dispatch_cookie_t.pkt_head_len
extern size_t get_header_len();

// Get full M2LPacket parcel
extern void get_packet(void* header);

// Write hdr to LMEM. Don't cluj_unload_send yet.
extern void stage_packet(void* header);

// cluj unload send
extern void send_packet();