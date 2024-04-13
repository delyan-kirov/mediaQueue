#include <sndfile.h>
#include <stdio.h>
#include <stdlib.h>

// Function to plot the audio signal using gnuplot
void plotAudioSignal(float *buffer, int numFrames) {
  FILE *gnuplotPipe = popen("gnuplot -persistent", "w");
  if (gnuplotPipe) {
    fprintf(gnuplotPipe, "plot '-' with lines\n");
    for (int i = 0; i < numFrames; i++) {
      fprintf(gnuplotPipe, "%f\n", buffer[i]);
    }
    fprintf(gnuplotPipe, "e\n");
    fflush(gnuplotPipe);
    fclose(gnuplotPipe);
  } else {
    printf("Error opening gnuplot pipe.\n");
  }
}

int main() {
  // Specify the path to your audio file
  const char *audioFilePath =
      "./Euphoria.flac"; // Replace with your audio file path

  // Open the audio file for reading
  SF_INFO sfInfo;
  SNDFILE *sndFile = sf_open(audioFilePath, SFM_READ, &sfInfo);
  if (!sndFile) {
    printf("Error opening file: %s\n", sf_strerror(sndFile));
    return 1;
  }

  // Read the audio data
  const int bufferSize = 1024; // Adjust the buffer size as needed
  float buffer[bufferSize];
  int numFramesRead;

  // Read audio frames until the end of the file
  while ((numFramesRead = sf_read_float(sndFile, buffer, bufferSize)) > 0) {
    // Plot the audio signal
    plotAudioSignal(buffer, numFramesRead);
  }

  // Close the audio file
  sf_close(sndFile);

  return 0;
}
