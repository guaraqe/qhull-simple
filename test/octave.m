points = load("sphere.txt", "-ascii");
h = convhulln(points);
f = fopen("sphere-hull-octave.txt", "w");
for i = 1:rows(h)
  fprintf(f, "%d", h(i, 1)-1);
  for j = 2:columns(h)
    fprintf(f, " %d", h(i, j)-1);
    end
  fprintf(f, "\n");
  end
fclose(f);