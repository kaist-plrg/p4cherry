#include <core.p4>
#include <v1model.p4>

header hdr {
  bit<8> a;
  bit<8> b;
  bit<8> c;
  bit<16> d;
}

error {
	Noerror}
struct headers {
    hdr h;
}

struct metadata {}

typedef bit<9> T;

type bit<9> new_T;
type bit<9> new_T2;


enum E {
	aa}

parser p(packet_in b, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        b.extract<hdr>(h.h);
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }

control ingress(inout headers h, inout metadata m, inout standard_metadata_t sm) {
    bit<8> z;
    action add()
    { h.h.d = (bit<16>)(h.h.a + h.h.b + h.h.c); sm.egress_spec = (bit<9>)0; }
    action sub()
    { h.h.c = (bit<8>)(h.h.b - h.h.a); sm.egress_spec = (bit<9>)1; }
    action a_with_control_params(in bit<9> x) { sm.egress_spec = (bit<9>)x; }

    table t {
        key = { h.h.a : exact; h.h.b : exact; h.h.c : exact; h.h.d : lpm;}
        actions = { add; sub; a_with_control_params(9w1);}
        const entries = {
          (8w0, 8w1, 8w2, 16w14) : a_with_control_params(9w2);
          
        }
        const default_action = add;
    }
    table tt {
        key = { h.h.a : exact; h.h.b : exact; h.h.c : exact; h.h.d : ternary;}
        actions = { add; sub; a_with_control_params(9w1);}
        const entries = {
          (8w0, 8w1, 8w2, 16w14) : sub;
          (8w0, 8w1, 8w2, 16w14) : add;
          (8w9, 8w7, 8w7, 16w49) : sub;
          // It should call error
          (8w1, 8w2, 8w2, 16w0 &&& 16w2) : a_with_control_params(9w2);
          
        }
        const default_action = add;
    }
    apply {  
	    if (tt.apply().miss) {
	    	t.apply();
	    }    
    }
}

control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<hdr>(h.h); }
}

