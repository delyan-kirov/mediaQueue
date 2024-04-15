#include <pipewire/pipewire.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  if (argc != 4) {
    fprintf(
        stderr,
        "Usage: %s <audio_file_path> <start_position_ms> <end_position_ms>\n",
        argv[0]);
    return 1;
  }

  // Convert start and end positions from milliseconds to nanoseconds
  uint64_t start_position_ns = atoi(argv[2]) * 1000000;
  uint64_t end_position_ns = atoi(argv[3]) * 1000000;

  pw_init(&argc, &argv);

  struct pw_main_loop *main_loop = pw_main_loop_new(NULL);
  struct pw_core *core = pw_core_new(pw_main_loop_get_loop(main_loop), NULL);
  if (!core) {
    fprintf(stderr, "Failed to create PipeWire core\n");
    return 1;
  }

  struct pw_stream *stream = pw_stream_new(core, "my-audio-stream", NULL);
  if (!stream) {
    fprintf(stderr, "Failed to create stream\n");
    return 1;
  }

  // Open the audio file
  int res = pw_stream_connect(
      stream, PW_DIRECTION_PLAYBACK, PW_STREAM_MODE_PLAYBACK,
      argv[1], // Path to audio file
      PW_STREAM_FLAG_AUTOCONNECT | PW_STREAM_FLAG_AUTOCONNECT |
          PW_STREAM_FLAG_DONT_RECONNECT,
      NULL, NULL);
  if (res < 0) {
    fprintf(stderr, "Failed to connect to audio file: %s\n",
            pw_get_error_string(res));
    return 1;
  }

  // Seek to the start position
  res = pw_stream_seek(stream, start_position_ns);
  if (res < 0) {
    fprintf(stderr, "Failed to seek to start position: %s\n",
            pw_get_error_string(res));
    return 1;
  }

  // Start the main loop
  pw_main_loop_run(main_loop);

  // Clean up resources
  pw_stream_destroy(stream);
  pw_core_destroy(core);
  pw_main_loop_destroy(main_loop);
  pw_deinit();

  return 0;
}
