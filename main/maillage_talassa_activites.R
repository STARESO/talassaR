#' ---
#' title : "talassaR - maillage_talassa_activites"
#' author : Aubin Woehrel
#' creation date : 2026-02-12
#' ---
#'
#' =============================================================================
#'
#' talassaR : Maillage des données activités
#'
#' Description :
#' Script permettant de passer des données activités ponctuelles au format
#' TALASSA vers des données activités intégrées au maillage TALASSA choisi.
#'
#' =============================================================================


## Réflexion transfos donia ----
ggplot(donia_obs, aes(x = taille, y = "")) +
  geom_beeswarm(method = "center") +
  theme_pubr() +
  labs(y = "")

ggplot(donia_obs, aes(x = duree_mouillage, y = "")) +
  geom_beeswarm(method = "center") +
  theme_classic() +
  labs(y = "")

g1 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_y_reverse()
ggMarginal(g1, type = "histogram", size = 3)

g2 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g2, type = "histogram", size = 3)

g3 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g3, type = "histogram", size = 3)

g4 <- ggplot(donia_obs, aes(x = taille, y = duree_mouillage)) +
  geom_point(size = 1) +
  theme_pubr() +
  scale_x_continuous(transform = "log10", limits = c(10, 200)) +
  scale_y_continuous(transform = scales::compose_trans("log10", "reverse"))
ggMarginal(g4, type = "histogram", size = 3)
