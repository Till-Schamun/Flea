# Pakete
library(ggplot2)
library(dplyr)
library(grid)  # für unit()

# --- Parameter ---
L <- 10   # Länge (m)
W <- 3    # Breite (m)
cam_size <- 0.25  # Grundmaß für Kameras (Dreieckskantenhöhe & Quadrat-Kantenlänge)

# --- Hilfsfunktionen ---------------------------------------------------------

# Punkte (x,y) um (cx,cy) drehen; angle in Grad (mathematisch positiv = gegen Uhrzeigersinn)
rotate <- function(x, y, cx, cy, angle){
  rad <- angle * pi/180
  xr <- (x - cx)*cos(rad) - (y - cy)*sin(rad) + cx
  yr <- (x - cx)*sin(rad) + (y - cy)*cos(rad) + cy
  data.frame(x = xr, y = yr)
}

# Dreieck: standardmäßig zeigt die BASIS nach +x (Spitze nach -x).
# -> 'angle' gibt die Richtung an, in die die BASIS zeigen soll.
triangle_df <- function(cx, cy, size = 0.25, angle = 0){
  # Basis liegt rechts vom Mittelpunkt, Spitze links
  base <- data.frame(
    x = c(cx - size, cx + 0,     cx + 0),
    y = c(cy,        cy - size/2, cy + size/2)
  )
  rotate(base$x, base$y, cx, cy, angle)
}

# Winkel (in Grad) vom Punkt (cx,cy) zu Ziel (tx,ty) relativ zu +x
angle_to <- function(cx, cy, tx, ty){
  atan2(ty - cy, tx - cx) * 180/pi
}

# --- Funktion zum Freisetzen von Kreisen ------------------------------------

add_circle <- function(p, x, y, size = 6, stroke = 1, fill = "white", color = "black") {
  p +
    geom_point(
      data  = data.frame(x = x, y = y),
      aes(x = x, y = y),
      shape = 21,      # Kreis
      size  = size,    # optische Größe
      stroke = stroke, # Randstärke
      fill  = fill,
      color = color
    )
}

# -----------------------------------------------------------------------------

# Abstände nach innen (y)
# Abstände nach innen (y)
y_inset_top <- 0.30  # oben 40 cm nach innen  (prüfe Kommentar vs. Wert)
y_inset_bot <- 0.25  # unten 30 cm nach innen

# Kamera-Positionen (3 oben, 3 unten)
xs <- c(0.6, L/2, 9.4)  # links, Mitte, rechts
pos <- tibble::tibble(
  x   = c(xs, xs),
  y   = c(rep(W - y_inset_top, 3), rep(0 + y_inset_bot, 3)),
  row = rep(c("top","bot"), each = 3)
)


# Geometrische Mitte als Ziel für äußere Kameras
target_x <- L/2
target_y <- W/2

# Winkel festlegen
pos <- pos %>%
  mutate(
    is_middle = abs(x - L/2) < 1e-9,
    base_angle = dplyr::case_when(
      is_middle & row == "top" ~ 180,          # oben Mitte: Basis nach links
      is_middle & row == "bot" ~   0,          # unten Mitte: Basis nach rechts
      TRUE ~ angle_to(x, y, target_x, target_y)   # äußere: Basis zeigt zur Mitte
    )
  )

# Polygone für Kamera-Dreiecke erzeugen
cams <- pos %>%
  rowwise() %>%
  do({
    tri <- triangle_df(.$x, .$y, size = cam_size, angle = .$base_angle)
    cbind(tri, id = paste0(.$row, "_x", sprintf("%.2f", .$x)))
  }) %>%
  ungroup()

# --- Quadrate an die Dreiecksspitzen setzen (gleiche Farbe & Größe) ----------

sq_size <- cam_size                 # Kantenlänge = cam_size
sq_gap  <- cam_size * 0.16          # kleiner Abstand proportional zu cam_size
mid_x   <- L/2

# Apex je Dreieck als eigene Spalten benennen und pos-Infos sauber joinen
tips <- cams %>%
  group_by(id) %>% slice(1) %>% ungroup() %>%
  rename(apex_x = x, apex_y = y) %>%
  left_join(pos %>% mutate(id = paste0(row, "_x", sprintf("%.2f", x))),
            by = "id")
# Jetzt existieren: apex_x/apex_y (Spitze des Polygons) UND x/y/row/is_middle (Kameraposition)

squares <- tips %>%
  rowwise() %>%
  mutate(
    # gewünschte Logik:
    # - außen links/rechts: seitlich neben die Spitze
    # - mitte oben: rechts vom Dreieck
    # - mitte unten: links vom Dreieck
    off_x = case_when(
      !is_middle & x <  mid_x ~ -(sq_gap + sq_size/2),  # links außen -> links
      !is_middle & x >  mid_x ~  (sq_gap + sq_size/2),  # rechts außen -> rechts
      is_middle   & row == "top" ~  (sq_gap + sq_size/2),  # Mitte oben -> rechts
      is_middle   & row == "bot" ~ -(sq_gap + sq_size/2),  # Mitte unten -> links
      TRUE ~ 0
    ),
    off_y = 0,  # bei der mittleren Kamera nicht vertikal versetzen
    cx = apex_x + off_x, cy = apex_y + off_y,
    xmin = cx - sq_size/2, xmax = cx + sq_size/2,
    ymin = cy - sq_size/2, ymax = cy + sq_size/2
  ) %>% ungroup()

# Tunnel-Umriss
rect <- data.frame(
  x = c(0, L, L, 0, 0),
  y = c(0, 0, W, W, 0)
)

# Maßpfeile (10 m / 3 m)
dim_segments <- dplyr::bind_rows(
  # horizontal (unten)
  data.frame(x = 0, xend = L, y = -0.15, yend = -0.15,
             label = "10 m", lx = L/2, ly = -0.35),
  # vertikal (links)
  data.frame(x = -0.15, xend = -0.15, y = 0, yend = W,
             label = "3 m",  lx = -0.45, ly = W/2)
)

# --- Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_path(data = rect, aes(x, y), linewidth = 0.6) +
  # Dreiecke (Kameras)
  geom_polygon(data = cams, aes(x, y, group = id), fill = "grey30") +
  # Quadrate (Gehäuse) – gleiche Farbe & Maß wie Dreiecke
  geom_rect(data = squares,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey30", color = NA) +
  # Maßlinien
  geom_segment(data = dim_segments,
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(3, "mm"), ends = "both"),
               linewidth = 0.4) +
  geom_text(data = dim_segments, aes(x = lx, y = ly, label = label), size = 5) +
  ggtitle("Schematic layout of 10 × 3 m tunnel with six cameras") +
  coord_equal(xlim = c(-0.8, L + 0.8), ylim = c(-0.8, W + 0.8), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# --- Beispiel: Kreise hinzufügen --------------------------------------------
p <- add_circle(p, x = 9.7, y = 2.35, size = 9)   # oben rechts
p <- add_circle(p, x = 0.3, y = 0.6, size = 8)   # unten links

# Anzeigen
print(p)
