SRCS := $(wildcard *.c)
OBJS := $(patsubst %.c,%.o,$(SRCS))
ROBJS := $(addprefix release/,$(OBJS))
DOBJS := $(addprefix debug/,$(OBJS))
POBJS := $(addprefix profile/,$(OBJS))
CC = gcc
CFLAGS = -Wall
LDFLAGS =
LIBS = -lm -lgmp

UNAME = $(shell uname -a)
ifneq (,$(findstring MINGW, $(UNAME)))
	EXT = .exe
else ifneq (,$(findstring MSYS, $(UNAME)))
	EXT = .exe
else
	EXT =
endif
TARGET = kiss$(EXT)

.PHONY: all clean test

all: release_dir release/$(TARGET) release_cp

release_dir:
	-mkdir release

release_cp:
	cp release/$(TARGET) .

debug: debug_dir debug/$(TARGET) debug_cp

debug_dir:
	-mkdir debug

debug_cp:
	cp debug/$(TARGET) .

profile: profile_dir profile/$(TARGET) profile_cp

profile_dir:
	-mkdir profile

profile_cp:
	cp profile/$(TARGET) .

release/$(TARGET): $(ROBJS)
	$(CC) $(LDFLAGS) -O3 -o $@ $^ $(LIBS)

debug/$(TARGET): $(DOBJS)
	$(CC) $(LDFLAGS) -O0 -g -o $@ $^ $(LIBS)

profile/$(TARGET): $(POBJS)
	$(CC) $(LDFLAGS) -O3 -pg -o $@ $^ $(LIBS)

release/%.o: %.c kiss.h
	$(CC) -c -O3 $(CFLAGS) $< -o $@

debug/%.o: %.c kiss.h
	$(CC) -c -O0 $(CFLAGS) -g $< -o $@

profile/%.o: %.c kiss.h
	$(CC) -c -O3 $(CFLAGS) -pg $< -o $@

clean:
	rm -f release/$(TARGET) debug/$(TARGET) profile/$(TARGET)
	rm -f $(OBJS) $(ROBJS) $(DOBJS) $(POBJS)
	rm -f newfile example.dat
