BUILD_DIR = ./build
CFLAGS = -O3 -Wall -Wextra -pedantic --std=gnu11 `pkg-config --cflags libbsd`
LDFLAGS = `pkg-config --libs libbsd`

STRIP = strip

all: $(BUILD_DIR)/brightness

$(BUILD_DIR)/brightness: src/main.c
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) $< $(LDFLAGS) -o $@
	$(STRIP) $@
