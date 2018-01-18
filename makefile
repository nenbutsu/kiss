SRCS := $(wildcard *.c)
OBJS := $(patsubst %.c,%.o,$(SRCS))
DOBJS := $(addprefix debug/,$(OBJS))
POBJS := $(addprefix profile/,$(OBJS))
CC = gcc
CFLAGS = -Wall -O3
LDFLAGS =
LIBS = -lm -lgmp

UNAME = $(shell uname -a)
ifneq (,$(findstring MINGW, $(UNAME)))
	EXT = .exe
else
	EXT =
endif
TARGET = kiss$(EXT)

.PHONY: all clean test

all: $(TARGET)

debug: debug_dir debug/$(TARGET)

debug_dir:
	-mkdir debug

profile: profile_dir profile/$(TARGET)

profile_dir:
	-mkdir profile

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

debug/$(TARGET): $(DOBJS)
	$(CC) $(LDFLAGS) -g -o $@ $^ $(LIBS)

profile/$(TARGET): $(POBJS)
	$(CC) $(LDFLAGS) -pg -o $@ $^ $(LIBS)

./%.o: %.c kiss.h
	$(CC) -c $(CFLAGS) $< -o $@

debug/%.o: %.c kiss.h
	$(CC) -c $(CFLAGS) -g $< -o $@

profile/%.o: %.c kiss.h
	$(CC) -c $(CFLAGS) -pg $< -o $@

clean:
	rm -f $(TARGET) $(OBJS) $(DOBJS) $(POBJS) newfile example.dat
