//__asm__(".macro nop\n\t.endm\n");

#include <cluj.h>
#include <endian.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#define LMEM_PARCEL_BASE 0

#define LMEM_HEAD_HEAD_BASE (512 - 64)
#define LMEM_HEAD_HEAD_SIZE 64
#define LMEM_HEAD_HEAD_LIMIT (LMEM_HEAD_HEAD_BASE + LMEM_HEAD_HEAD_SIZE)

#define LMEM_HEAD_TAIL_BASE 512
#define LMEM_HEAD_TAIL_SIZE 512
#define LMEM_HEAD_TAIL_LIMIT (LMEM_HEAD_TAIL_BASE + LMEM_HEAD_TAIL_SIZE)

#if 0
#define ASMLOG(MSG)                                                            \
  do {                                                                         \
    __asm__ volatile(MSG);                                                     \
  } while (0)
#else
#define ASMLOG(MSG)                                                            \
  do {                                                                         \
  } while (0)
#endif

struct l2mpkthdr {
  uint32_t parcel_type : 4;
  uint32_t has_stats : 1;
  uint32_t mcinc : 1;
  uint32_t has_offset : 1;
  uint32_t has_drop_hash : 1;
  uint32_t mcdec : 1;
  uint32_t priority : 1;
  uint32_t tail_offset : 9;
  uint32_t packet_table_entry : 13;
  uint32_t color : 3;
  uint32_t queuing_opcode : 2;
  uint32_t queuing_system : 3;
  uint32_t lifetime : 4;
  uint32_t queue_number : 20;
};

#define L2M_PARCEL_TYPE_PACKET 0x0
#define L2M_PARCEL_TYPE_BACKPRESSURE 0x2
#define L2M_PARCEL_TYPE_PACKET_DATA_READ 0x3
#define L2M_PARCEL_TYPE_PACKET_DATA_WRITE 0x4
#define L2M_PARCEL_TYPE_BULK_BACKPRESSURE 0xa

#define L2M_QOP_ENQUEUE_NO_DROP 0
#define L2M_QOP_DROP 1
#define L2M_QOP_ENQUEUE_NORMAL 2
#define L2M_QOP_ENQUEUE_USE_PREVIOUS 3

struct etherhdr {
  uint8_t ether_dhost[6];
  uint8_t ether_shost[6];
  uint16_t ether_type;
};

#define ETHERTYPE_IP 0x0800 /* IP */

struct iphdr {
#if __BYTE_ORDER == __LITTLE_ENDIAN
  unsigned int ihl : 4;
  unsigned int version : 4;
#elif __BYTE_ORDER == __BIG_ENDIAN
  unsigned int version : 4;
  unsigned int ihl : 4;
#else
#error "Please fix <bits/endian.h>"
#endif
  uint8_t tos;
  uint16_t tot_len;
  uint16_t id;
  uint16_t frag_off;
  uint8_t ttl;
  uint8_t protocol;
  uint16_t check;
  uint32_t saddr;
  uint32_t daddr;
  /*The options start here. */
};

#define IPPROTO_UDP 17 /* User Datagram Protocol.  */

struct udphdr {
  uint16_t uh_sport; /* source port */
  uint16_t uh_dport; /* destination port */
  uint16_t uh_ulen;  /* udp length */
  uint16_t uh_sum;   /* udp checksum */
};

struct FlowCounter {
  char name[32];
  uint64_t bytes;
  uint64_t packets;
} counters[256];

#define OP_CNTR_INIT 0
#define OP_CNTR_ADJUST 1
#define OP_CNTR_READ 2

void *memcpy_both_aligned(void *d, const void *s, size_t n) {
  uint8_t *dst = __builtin_assume_aligned(d, sizeof(uint32_t));
  const uint8_t *src = __builtin_assume_aligned(s, sizeof(uint32_t));
  const uint8_t *limit = src + (n & ~3);

  while (src != limit) {
    *(uint32_t *)dst = *(const uint32_t *)src;
    src += 4;
    dst += 4;
  }
  if (n & 2) {
    *(uint16_t *)dst = *(const uint16_t *)src;
    src += 2;
    dst += 2;
  }
  if (n & 1) {
    *dst = *src;
  }

  return d;
}

void *memcpy_src_aligned(void *d, const void *s, size_t n) {
  uint8_t *dst = d;
  const uint8_t *src = __builtin_assume_aligned(s, sizeof(uint32_t));
  const uint8_t *limit = src + (n & ~3);

  while (src != limit) {
    uint32_t data = *(uint32_t *)src;
    *dst++ = data >> 24;
    *dst++ = data >> 16;
    *dst++ = data >> 8;
    *dst++ = data >> 0;
    src += 4;
  }
  if (n & 2) {
    uint32_t data = *(uint16_t *)src;
    *dst++ = data >> 8;
    *dst++ = data >> 0;
    src += 2;
  }
  if (n & 1) {
    *dst = *src;
  }

  return d;
}

void *memcpy(void *d, const void *s, size_t n) {
  uint8_t *dst = d;
  const uint8_t *src = s;

  if (((uintptr_t)dst & 3) == ((uintptr_t)src & 3)) {
    /* src and dst have same alignment */
    if (n >= 1 && ((uintptr_t)src & 1)) {
      *dst++ = *src++;
      n--;
    }
    if (n >= 2 && ((uintptr_t)src & 2)) {
      *(uint16_t *)dst = *(const uint16_t *)src;
      src += 2;
      dst += 2;
      n -= 2;
    }
    memcpy_both_aligned(dst, src, n);
  } else {
    /* src and dst have different alignment */
    /* copy enough to get src 32-bit aligned */
    if (n >= 1 && ((uintptr_t)src & 1)) {
      *dst++ = *src++;
      n--;
    }
    if (n >= 2 && ((uintptr_t)src & 2)) {
      uint32_t data = *(const uint16_t *)src;
      *dst++ = data >> 8;
      *dst++ = data >> 0;
      src += 2;
      n -= 2;
    }
    memcpy_src_aligned(dst, src, n);
  }

  return d;
}

static inline const void *lmem_copyin_aligned(const void *src, size_t count) {
  const uint8_t *p = __builtin_assume_aligned(src, sizeof(uint32_t));
  const uint8_t *q = p + (count & ~3);

  while (p != q) {
    cluj_write_lmem_4B_ai(*(uint32_t *)p);
    p += sizeof(uint32_t);
  }

  if (count & 2) {
    cluj_write_lmem_2B_ai(*(uint16_t *)p);
    p += sizeof(uint16_t);
  }

  if (count & 1) {
    cluj_write_lmem_1B_ai(*(uint8_t *)p);
    p += sizeof(uint16_t);
  }

  return p;
}

static inline const void *lmem_copyin(const void *src, size_t count) {
  const uint8_t *p = src;

  if (count == 0)
    return p;

  if (((uintptr_t)p) & 1) {
    cluj_write_lmem_1B_ai(*(uint8_t *)p);
    p += 1;
    count -= 1;

    if (count == 0)
      return p;
  }

  if (((uintptr_t)p) & 2) {
    cluj_write_lmem_2B_ai(*(uint16_t *)p);
    p += 2;
    count -= 2;

    if (count == 0)
      return p;
  }

  return lmem_copyin_aligned(p, count);
}

static uint16_t ipsum_words(const void *p_, size_t len) {
  const uint32_t *p = __builtin_assume_aligned(p_, sizeof(uint32_t));
  const uint32_t *q = p + len;
  uint32_t sum32 = 0;

  while (p != q) {
    uint32_t data32 = *p++;
    sum32 += (uint16_t)(data32 >> 0);
    sum32 += (uint16_t)(data32 >> 16);
  }

  sum32 = (uint16_t)sum32 + (uint16_t)(sum32 >> 16);
  sum32 = (uint16_t)sum32 + (uint16_t)(sum32 >> 16);

  return ~sum32;
}

static inline uint32_t prepend(uint32_t addr, size_t size) {
  addr -= size;
  cluj_set_wp(3, addr << 3);
  return addr;
}

static void lmem_send_tail(size_t head_tail_len) {
  size_t head_head_len = 0;

  unload_send_addr_t addr = {
      .u.packet = 0,
      .u.interface = 0,
      .u.head_tail_len = head_tail_len,
      .u.head_head_len = head_head_len,
      .u.first_tail_num_seg = 0, // not used
      .u.zone = 0,               // filled in by HW
  };

  unload_send_data_t data = {
      .u.first_tail_addr = 0,    // not used
      .u.first_tail_dealloc = 0, // not used
      .u.parcel_len = 0,         // not used
      .u.head_head_addr = LMEM_HEAD_HEAD_BASE,
      .u.head_tail_addr = LMEM_HEAD_TAIL_BASE,
  };

  cluj_unload_send(addr, data);

  cluj_set_wp(3, LMEM_HEAD_TAIL_BASE << 3);
}

static void lmem_send(uint32_t head_head_addr, size_t head_head_len,
                      uint32_t head_tail_addr, size_t head_tail_len) {
  unload_send_addr_t addr = {
      .u.packet = 1,
      .u.interface = 0,
      .u.head_tail_len = head_tail_len,
      .u.head_head_len = head_head_len,
      .u.first_tail_num_seg = 0, // not used
  };

  unload_send_data_t data = {
      .u.first_tail_addr = 0,    // not used
      .u.first_tail_dealloc = 0, // not used
      .u.parcel_len = 0,         // not used
      .u.head_head_addr = head_head_addr,
      .u.head_tail_addr = head_tail_addr,
  };

  cluj_unload_send(addr, data);
}

static bool send_drop(void) { // DOUBT: What is this function doing?
  uint32_t lmaddr = LMEM_HEAD_HEAD_LIMIT;

  struct l2mpkthdr l2mpkthdr = {
      .parcel_type = L2M_PARCEL_TYPE_PACKET,
      .has_stats = 0,
      .mcinc = 1,
      .has_offset = 0,
      .has_drop_hash = 0,
      .mcdec = 1,
      .priority = 0,
      .tail_offset = 0x1ff, // parcel is the entire packet
      .packet_table_entry = 0,
      .color = 0,
      .queuing_opcode = L2M_QOP_DROP,
      .queuing_system = 0,
      .lifetime = 0,
      .queue_number = 0,
  };
  lmaddr = prepend(lmaddr, sizeof(l2mpkthdr) + sizeof(uint16_t)); // DOUBT: Why is sizeof(uint16_t) added here?
  lmem_copyin_aligned(&l2mpkthdr, sizeof(l2mpkthdr));
  cluj_write_lmem_2B_ai(1); // mccount // DOUBT: Why is 1 the value being written here?

  size_t head_head_len = LMEM_HEAD_HEAD_LIMIT - lmaddr;
  lmem_send(lmaddr, head_head_len, 0, 0);
}

static bool send_counter(int index) {
  uint32_t lmaddr = LMEM_HEAD_HEAD_LIMIT;
  uint64_t packet_counter = counters[index].packets;
  uint64_t byte_counter = counters[index].bytes;

  struct l2mpkthdr l2mpkthdr = {
      .parcel_type = L2M_PARCEL_TYPE_PACKET,
      .has_stats = 0,
      .mcinc = 1,
      .has_offset = 0,
      .has_drop_hash = 0,
      .mcdec = 1,
      .priority = 0,
      .tail_offset = 0x1ff, // parcel is the entire packet
      .packet_table_entry = 0,
      .color = 0,
      .queuing_opcode = L2M_QOP_DROP,
      .queuing_system = 0,
      .lifetime = 0,
      .queue_number = 0,
  };
  lmaddr = prepend(lmaddr, sizeof(l2mpkthdr) + sizeof(uint16_t));
  lmem_copyin_aligned(&l2mpkthdr, sizeof(l2mpkthdr));
  cluj_write_lmem_2B_ai(1); // mccount
  cluj_write_lmem_4B_ai((packet_counter >> 32) & 0xffffffff); // DOUBT: Explain memory access here.
  cluj_write_lmem_4B_ai((packet_counter >> 0) & 0xffffffff);
  cluj_write_lmem_4B_ai((byte_counter >> 32) & 0xffffffff);
  cluj_write_lmem_4B_ai((byte_counter >> 0) & 0xffffffff);

  size_t head_head_len = (LMEM_HEAD_HEAD_LIMIT - lmaddr) +
                         sizeof(packet_counter) + sizeof(byte_counter); // DOUBT: Explain memory access here
  lmem_send(lmaddr, head_head_len, 0, 0);
}

typedef struct pbj_ostream {
  size_t max_len;
  size_t bytes_written;
} pbj_ostream_t;

static bool pkt_flush(pbj_ostream_t *stream) {
  uint32_t lmaddr = LMEM_HEAD_HEAD_LIMIT;
  size_t total_len = stream->bytes_written;

  /* UDP */
  total_len += sizeof(struct udphdr);
  struct udphdr udphdr = {
      .uh_sport = 4501,
      .uh_dport = 4502,
      .uh_ulen = total_len,
      .uh_sum = 0x0000, /* don't want to have to compute the whole checksum */
  };
  lmaddr = prepend(lmaddr, sizeof(udphdr));
  lmem_copyin_aligned(&udphdr, sizeof(udphdr));

  /* IP */
  total_len += sizeof(struct iphdr);
  struct iphdr iphdr = {
      .version = 4,
      .ihl = 5,
      .tos = 0,
      .tot_len = total_len,
      .id = 0,
      .frag_off = 0,
      .ttl = 32,
      .protocol = IPPROTO_UDP,
      .check = 0, /* computed below */
      .saddr = (192 << 24) | (168 << 16) | (0 << 8) | (1 << 0),
      .daddr = (192 << 24) | (168 << 16) | (0 << 8) | (254 << 0),
  };
  ASMLOG("# !!! Computing IP checksum !!!");
  iphdr.check = ipsum_words(&iphdr, sizeof(iphdr) / sizeof(uint32_t));
  ASMLOG("# !!! Copying IP header to LMEM !!!");
  lmaddr = prepend(lmaddr, sizeof(iphdr));
  lmem_copyin_aligned(&iphdr, sizeof(iphdr));

  /* Ether */
  static const struct etherhdr etherhdr = {
      .ether_dhost = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05},
      .ether_shost = {0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b},
      .ether_type = ETHERTYPE_IP,
  };
  lmaddr = prepend(lmaddr, sizeof(etherhdr));
  lmem_copyin_aligned(&etherhdr, sizeof(etherhdr));

  /* L2M */
  struct l2mpkthdr l2mpkthdr = {
      .parcel_type = L2M_PARCEL_TYPE_PACKET,
      .has_stats = 0,
      .mcinc = 1,
      .has_offset = 0,
      .has_drop_hash = 0,
      .mcdec = 1,
      .priority = 0,
      .tail_offset = 0x1ff,
      .packet_table_entry = 0,
      .color = 0,
      .queuing_opcode = L2M_QOP_ENQUEUE_NORMAL,
      .queuing_system = 0,
      .lifetime = 0,
      .queue_number = 1500,
  };
  lmaddr = prepend(lmaddr, sizeof(l2mpkthdr) + sizeof(uint16_t));
  lmem_copyin_aligned(&l2mpkthdr, sizeof(l2mpkthdr));
  cluj_write_lmem_2B_ai(1); // mccount

  size_t head_head_len = LMEM_HEAD_HEAD_LIMIT - lmaddr;

  if (stream->bytes_written < LMEM_HEAD_TAIL_SIZE) {
    /* head-only packet */
    lmem_send(lmaddr, head_head_len, LMEM_HEAD_TAIL_BASE,
              stream->bytes_written);
  } else {
    /* we have flushed a tail chunk, so we must complete the tail, and then
     * attach the head */
    size_t residue = stream->bytes_written & (LMEM_HEAD_TAIL_SIZE - 1);

    /* flush any tail residue */
    if (residue > 0)
      lmem_send_tail(residue);

    /* send packet */
    lmem_send(lmaddr, head_head_len, 0, 0);
  }

  return true;
}

static inline void pbj_write(pbj_ostream_t *stream, const void *src,
                             size_t count) {
  size_t avail = LMEM_HEAD_TAIL_LIMIT - (cluj_get_wp(3) >> 3);

  stream->bytes_written += count;

  if (count >= avail) {
    src = lmem_copyin(src, avail);
    lmem_send_tail(LMEM_HEAD_TAIL_SIZE);
    count -= avail;

    avail = LMEM_HEAD_TAIL_SIZE;

    while (count >= avail) {
      src = lmem_copyin(src, avail);
      lmem_send_tail(LMEM_HEAD_TAIL_SIZE);
      count -= avail;
    }
  }

  if (count > 0) {
    src = lmem_copyin(src, count);
  }
}

struct divmod {
  uint32_t q, r;
};

static struct divmod divmod(uint32_t n, uint32_t d) {
#if 1 // disable when we have divu/multu
  uint32_t x = d;
  uint32_t q = 0;

  while (x < n && (int32_t)x > 0)
    x <<= 1;
  while (n >= d) {
    q <<= 1;
    if (n >= x) {
      n -= x;
      q |= 1;
    }
    x >>= 1;
  }
  return (struct divmod){.q = q, .r = n};
#else
  return (struct divmod){.q = n / d, .r = n % d};
#endif
}

void init_counters(void) {
  int i;

  for (i = 0; i < 0x100; i++) {
    struct FlowCounter *fc = &counters[i];
    memcpy(fc->name, "counter-", 8);
    char *p;
    struct divmod r = {.q = i};
    if (r.q < 10) {
      p = &fc->name[10];
    } else if (r.q < 100) {
      p = &fc->name[11];
    } else {
      p = &fc->name[12];
    }
    *--p = '\0';
    do {
      r = divmod(r.q, 10);
      *--p = '0' + r.r;
    } while (r.q);
    fc->bytes = 0;
    fc->packets = 0;
  }
}

int main(uint32_t context) {
  uint8_t op; // Where are these values stored? Stack in LMEM?
  uint32_t index;
  uint32_t count;
  uint32_t value;

  cluj_set_wp(3, (LMEM_PARCEL_BASE + 12) << 3);
  // Answer: This is starting at 12 to skip past the dispatch cookie and L2MCookie which are 12 bytes together.
  // Answer: This must be bit granularity because the shift by 3 is to convert from bit granularity to byte granularity.

  op = cluj_read_lmem_1B_ai();

  switch (op) {
  case OP_CNTR_INIT:
    init_counters();
    send_drop(); // DOUBT: Why? What is this doing?
    break;

  case OP_CNTR_ADJUST:
    index = cluj_read_lmem_1B_ai();
    value = cluj_read_lmem_4B_ai();
    counters[index].packets++;
    counters[index].bytes += value;
    send_drop();
    break;

  case OP_CNTR_READ:
    index = cluj_read_lmem_1B_ai();
    count = cluj_read_lmem_1B_ai();
    // XXX: note that this currently only reads a single counter
    send_counter(index);
#if 0
    if (!send_agent_notif(index, count))
      send_drop();
#endif
    break;
  }

  return 0;
}
