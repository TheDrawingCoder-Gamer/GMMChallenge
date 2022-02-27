CC:=gcc


SRC_PATHS:=src
C_SRCS:=$(foreach PATH,$(SRC_PATHS), $(wildcard $(PATH)/*.c))
C_OBJS:=${C_SRCS:.c=.o}
OBJS:=C_OBJS
CURL_FLAGS:=$(shell curl-config --cflags)
CURL_LIBS:=$(shell curl-config --libs)
SANITY_FLAGS:=-Wall -pedantic -Werror
CFLAGS+= $(CURL_FLAGS) $(shell pkg-config --cflags json-c)
LDFLAGS += $(shell pkg-config --libs json-c)  $(CURL_LIBS) 
all: 
	$(CC) $(CFLAGS)  $(C_SRCS) -o cligmm $(LDFLAGS)