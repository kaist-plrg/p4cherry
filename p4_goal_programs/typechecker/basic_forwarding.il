extern packet_in {
    void extract<T>(out T hdr);
    void extract<T>(out T variableSizeHeader, in bit<32> variableFieldSizeInBits);
    T lookahead<T>();
    void advance(in bit<32> sizeInBits);
    bit<32> length();
}
extern packet_out {
    void emit<T>(in T hdr);
}
extern void verify(in bool check, in error toSignal);
action NoAction()
{

}
extern bool static_assert( bool check,  string message);
extern bool static_assert( bool check);
const bit<32> __v1model_version = 32w20180101;
extern counter {
    void count(in bit<32> index);
    counter( bit<32> size,  enum CounterType type);
}
extern direct_counter {
    void count();
    direct_counter( enum CounterType type);
}
extern meter {
    void execute_meter<T>(in bit<32> index, out T result);
    meter( bit<32> size,  enum MeterType type);
}
extern direct_meter<T> {
    void read(out T result);
    direct_meter( enum MeterType type);
}
extern register<T> {
    void read(out T result, in bit<32> index);
    void write(in bit<32> index, in T value);
    register( bit<32> size);
}
extern action_profile {
    action_profile( bit<32> size);
}
extern void random<T>(out T result, in T lo, in T hi);
extern void digest<T>(in bit<32> receiver, in T data);
extern void mark_to_drop();
extern void mark_to_drop(inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata);
extern void hash<O, T, D, M>(out O result, in enum HashAlgorithm algo, in T base, in D data, in M max);
extern action_selector {
    action_selector( enum HashAlgorithm algorithm,  bit<32> size,  bit<32> outputWidth);
}
extern Checksum16 {
    bit<16> get<D>(in D data);
    Checksum16();
}
extern void verify_checksum<T, O>(in bool condition, in T data, in O checksum,  enum HashAlgorithm algo);
extern void update_checksum<T, O>(in bool condition, in T data, inout O checksum,  enum HashAlgorithm algo);
extern void verify_checksum_with_payload<T, O>(in bool condition, in T data, in O checksum,  enum HashAlgorithm algo);
extern void update_checksum_with_payload<T, O>(in bool condition, in T data, inout O checksum,  enum HashAlgorithm algo);
extern void clone(in enum CloneType type, in bit<32> session);
extern void resubmit<T>(in T data);
extern void resubmit_preserving_field_list( bit<8> index);
extern void recirculate<T>(in T data);
extern void recirculate_preserving_field_list( bit<8> index);
extern void clone3<T>(in enum CloneType type, in bit<32> session, in T data);
extern void clone_preserving_field_list(in enum CloneType type, in bit<32> session,  bit<8> index);
extern void truncate(in bit<32> length);
extern void assert(in bool check);
extern void assume(in bool check);
extern void log_msg( string msg);
extern void log_msg<T>( string msg, in T data);
package V1Switch<H, M>( parser ( b extern packet_in { advance(sizeInBits) : extern_method<>(in sizeInBits bit<32>) -> void
extract(hdr) : extern_method<T>(out hdr T) -> void
extract(variableSizeHeader, variableFieldSizeInBits) : extern_method<T>(out variableSizeHeader T, in variableFieldSizeInBits bit<32>) -> void
length() : extern_method<>() -> bit<32>
lookahead() : extern_method<T>() -> T }, out parsedHdr H, inout meta M, inout standard_metadata struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> }) p,  control (inout hdr H, inout meta M) vr,  control (inout hdr H, inout meta M, inout standard_metadata struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> }) ig,  control (inout hdr H, inout meta M, inout standard_metadata struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> }) eg,  control (inout hdr H, inout meta M) ck,  control ( b extern packet_out { emit(hdr) : extern_method<T>(in hdr T) -> void }, in hdr H) dep);
const bit<16> TYPE_IPV4 = 16w2048;
parser MyParser( extern packet_in { advance(sizeInBits) : extern_method<>(in sizeInBits bit<32>) -> void
extract(hdr) : extern_method<T>(out hdr T) -> void
extract(variableSizeHeader, variableFieldSizeInBits) : extern_method<T>(out variableSizeHeader T, in variableFieldSizeInBits bit<32>) -> void
length() : extern_method<>() -> bit<32>
lookahead() : extern_method<T>() -> T } packet, out struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } } hdr, inout struct metadata {  } meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata)() {
    state start
      {
        transition parse_ethernet;
      }
    state parse_ethernet
      {
        packet.extract<header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }>(hdr.ethernet);
        transition select (hdr.ethernet.etherType) {
              (TYPE_IPV4): parse_ipv4;
              (default): accept;
          };
      }
    state parse_ipv4
      {
        packet.extract<header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> }>(hdr.ipv4);
        transition accept;
      }
}
control MyVerifyChecksum(inout struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } } hdr, inout struct metadata {  } meta)() {
  apply
  {

  }
}
control MyIngress(inout struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } } hdr, inout struct metadata {  } meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata)() {
    action drop()
    {
      mark_to_drop();
    }
    action ipv4_forward( bit<48> dstAddr,  bit<9> port)
    {
      standard_metadata.egress_spec = port;
      hdr.ethernet.srcAddr = hdr.ethernet.dstAddr;
      hdr.ethernet.dstAddr = dstAddr;
      hdr.ipv4.ttl = ((hdr.ipv4.ttl) - (((bit<8>) (1))));
    }
    table ipv4_lpm {
      key = {
        hdr.ipv4.dstAddr : lpm;
      }
      actions = {
        ipv4_forward;
        drop;
        NoAction;
      }

      default_action =         NoAction;
         size = 1024;
    }  apply
  {
    if (hdr.ipv4.isValid())
    {
      ipv4_lpm.apply();
    }
  }
}
control MyEgress(inout struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } } hdr, inout struct metadata {  } meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata)() {
  apply
  {

  }
}
control MyComputeChecksum(inout struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } } hdr, inout struct metadata {  } meta)() {
  apply
  {
    update_checksum<tuple<bit<4>,
bit<4>, bit<8>, bit<16>, bit<16>, bit<3>, bit<13>, bit<8>, bit<8>, bit<32>,
bit<32>>, bit<16>>(hdr.ipv4.isValid(), { hdr.ipv4.version, hdr.ipv4.ihl, hdr.ipv4.diffserv, hdr.ipv4.totalLen, hdr.ipv4.identification, hdr.ipv4.flags, hdr.ipv4.fragOffset, hdr.ipv4.ttl, hdr.ipv4.protocol, hdr.ipv4.srcAddr, hdr.ipv4.dstAddr }, hdr.ipv4.hdrChecksum, HashAlgorithm.csum16);
  }
}
control MyDeparser( extern packet_out { emit(hdr) : extern_method<T>(in hdr T) -> void } packet, in struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } } hdr)() {
  apply
  {
    packet.emit<header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }>(hdr.ethernet);
    packet.emit<header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> }>(hdr.ipv4);
  }
}
V1Switch<struct headers { ethernet = header ethernet_t { dstAddr = bit<48>; srcAddr = bit<48>; etherType = bit<16> }; ipv4 = header ipv4_t { version = bit<4>; ihl = bit<4>; diffserv = bit<8>; totalLen = bit<16>; identification = bit<16>; flags = bit<3>; fragOffset = bit<13>; ttl = bit<8>; protocol = bit<8>; hdrChecksum = bit<16>; srcAddr = bit<32>; dstAddr = bit<32> } }, struct metadata {  }>(MyParser(), MyVerifyChecksum(), MyIngress(), MyEgress(), MyComputeChecksum(), MyDeparser()) main;
