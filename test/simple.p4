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

/*
control ctrl() {
    apply {
        bit<32> a = 0;
        bit<32> b = 0;
        bit<32> c = 0;

        a = 0;
        b = 1;
        c = 2;
        if (a == 0) {
            b = 2;
            c = 3;
        } else {
            b = 3;
            c = 4;
        }
        c = 5;
    }
}

control noop();
package p(noop _n);
p(ctrl()) main;
*/

const bit<32> glob = 32w42;

struct S {
    bit<32> x;
}

control c(inout bit<32> b) {
    apply {
        S s1;
        S s2;
        s2 = { 0 };
        s1 = s2;
        s2 = s1;
        b = s2.x;
    }
}

control proto(inout bit<32> _b);
package top(proto _p);

top(c()) main;
