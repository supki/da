#include <argp.h>
#include <bsd/stdlib.h>
#include <stdio.h>
#include <string.h>

static const char* BRIGHTNESS_DEFAULT_PATH = "/sys/class/backlight/intel_backlight/brightness";
static int BRIGHTNESS_MIN = 10; // I can't see shit with anything less than 10, 0 is simply a black screen.
static int BRIGHTNESS_MAX = 200; // While the actual maximum value is 93-fucking-7, 100 is more than enough to melt the eyes.

static error_t parse_opt(int key, char* arg, struct argp_state* state);
static int get_brightness(const char* path);
static int set_brightness(const char* path, int value);
static void close_file(FILE**);

static char doc[] = "Set the brightness of the screen";
static char args_doc[] = "BRIGHTNESS";
static struct argp_option options[] = {
  {"sys", 's', "FILE", 0, "Path of the file in /sys writing to which sets the brightness.", 0},
  { 0 },
};
static struct argp argp = { options, parse_opt, args_doc, doc, 0, 0, 0 };

struct arguments {
  const char* path;
  int value;
};

int main(int argc, char** argv) {
  struct arguments arguments = {.path = BRIGHTNESS_DEFAULT_PATH};

  argp_parse(&argp, argc, argv, 0, 0, &arguments);

  if (arguments.value) {
    return set_brightness(arguments.path, arguments.value);
  } else {
    return get_brightness(arguments.path);
  }
}

static error_t parse_opt(int key, char* arg, struct argp_state* state) {
  struct arguments* arguments = state->input;

  switch (key) {
    case 's':
      arguments->path = arg;
      break;

    case ARGP_KEY_ARG:
      if (state->arg_num > 0) {
        argp_error(state, "Too many arguments.");
      } else {
        const char* err;

        arguments->value = strtonum(arg, BRIGHTNESS_MIN, BRIGHTNESS_MAX, &err);

        if (err) {
          argp_error(state, "Brightness is %s (must be between %d and %d).", err, BRIGHTNESS_MIN, BRIGHTNESS_MAX);
        }
      }
      break;

    default:
       return ARGP_ERR_UNKNOWN;
  }

  return 0;
}

static int get_brightness(const char* path) {
  __auto_type h __attribute__((cleanup(close_file))) = fopen(path, "r");

  if (!h) {
    fprintf(stderr, "Couldn't open %s: %s.\n", path, strerror(errno));

    return EXIT_FAILURE;
  } else {
    __auto_type CHUNK = 4096;
    char buf[CHUNK];
    size_t nread;

    while ((nread = fread(buf, 1, sizeof(buf), h)) > 0) {
      fwrite(buf, 1, nread, stdout);
    }

    if (ferror(h)) {
      fprintf(stderr, "Couldn't read from %s: %s.\n", path, strerror(errno));

      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

static int set_brightness(const char* path, int value) {
  __auto_type h __attribute__((cleanup(close_file))) = fopen(path, "w");

  if (!h) {
    fprintf(stderr, "Couldn't open %s: %s.\n", path, strerror(errno));

    return EXIT_FAILURE;
  } else {
    __auto_type nwritten = fprintf(h, "%d\n", value);

    if (!nwritten) {
      fprintf(stderr, "Couldn't write to %s: %s.\n", path, strerror(errno));

      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

static void close_file(FILE** h) {
  if (*h) {
    fclose(*h);
  }
}
