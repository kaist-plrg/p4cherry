/*
Copyright 2013-present Barefoot Networks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <core.p4>
#include <v1model.p4>

header hdr {
    bit<8>  e;
    bit<16> t;
    bit<8>  l;
    bit<8>  r;
    bit<8>  v;
}

struct headers {
    hdr h;
}
struct metadata {}

parser p(packet_in b, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        b.extract<hdr>(h.h);
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }
control update(inout headers h, inout metadata m) { apply {} }
control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }
control deparser(packet_out b, in headers h) { apply { b.emit<hdr>(h.h); } }

control ingress(inout headers h, inout metadata m, inout standard_metadata_t standard_meta) {

    action a() { standard_meta.egress_spec = 9w0; }
    action a_with_control_params(bit<9> x) { standard_meta.egress_spec = x; }

    table t_valid {

  	key = {
            h.h.isValid() : exact;
            h.h.e         : exact;
        }

	actions = {
            a;
            a_with_control_params;
        }

	default_action = a;

        const entries = {
            (true,  0x01) : a_with_control_params(1);
            (false, 0x02) : a_with_control_params(2);
        }
    }

    apply {
        t_valid.apply();
    }
}


V1Switch<headers, metadata>(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
