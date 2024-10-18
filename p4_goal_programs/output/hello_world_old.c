#include<stdio.h>

typedef struct standard_metadata {
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
} standard_metadata;
typedef struct empty {

} empty;
typedef struct flag {
  char ok;
} flag;
void MyParser () { }
void MyVerifyChecksum (empty* hdr,flag* meta) { }
void MyIngress (empty* hdr,flag* meta,standard_metadata* standard_metadata)
{
  standard_metadata->egress_spec = 2;
}
void MyEgress (empty* hdr,flag* meta,standard_metadata* standard_metadata)
{ }
void MyUpdateChecksum (empty* hdr,flag* meta) { }
void MyDeparser (empty* hdr) { }
int main () {
  empty h;
  flag m;  
  standard_metadata sm;
  MyParser();
  MyVerifyChecksum(&h, &m);
  MyIngress(&h, &m, &sm);
  MyEgress(&h, &m, &sm);
  MyUpdateChecksum(&h, &m);
  MyDeparser(&h);
  printf("egress_spec: %d\n", sm.egress_spec);
}
