# Règle de contrôle d'exploitation (HCR) pour la pêcherie à la langouste dans la ZEE de Saint-Paul et Amsterdam

Shiny app pour tester la réaction de différentes paramétrisations de l'HCR.

## Description des paramètres

### 1. Scénario d'abondance

- **Scénario** :
  - Historique puis chute : réaction de l'HCR dans le cas de la chute d'abondance observée dans les années 1980. On considère la série historique des CPUE et captures comme base.
  - Historique puis stabilisation : réaction de l'HCR dans le cas de la stabilisation de l'abondance à une valeur supérieure à la cible. On considère la série historique des CPUE et captures comme base.
  - **Constante puis chute** : réaction de l'HCR dans le cas d'une chute d'abondance rapide. On part d'un équilibre au niveau de la valeur cible et fait chuter l'abondance de 15 % par an. Noter que dans le cas de ce scénario, les choix d'île et de zone n'ont pas d'influence.
  - Oscillations : réaction de l'HCR dans le cas d'oscillations de l'abondance.
- **Ile** : Situation à Amsterdam ou Saint-Paul.
- **Zone** : Plateau péri-insulaire complet ou application par zone (côtière ou profonde).
  
### 2. HCR

#### 2.1.Fixation du TAC

- (i) Valeur du TAC :
  - **Moyenne de I sur** : Nombre d'années sur lesquelles on calcule l'indicateur d'abondance récente.
  - **Calcul de la moyenne** :
    - Moyenne : moyenne classique utilisée pour calculer l'indicateur d'abondance récente
    - Approche de précaution : exemple pour 3 ans. On calcule la moyenne sur les 3 dernières années, sur les deux dernières années et la valeur de la dernière année. On garde ensuite la valeur la plus basse comme indicateur d'abondance récente.
  - **Palier inférieur** : Pourcentage du seuil de limitation d'une baisse de captures.
  - **Palier supérieur** : Pourcentage du seuil de limitation d'une augmentation de captures.
- (ii) Périodicité :
  - **Application tous les** : Toutes les combien d'années on applique l'HCR.
  - **Première application** : Est-ce qu'on applique l'HCR pour la première fois dès l'année prochaine, dans 2 ans ou dans 3 ans. Permet de jouer sur le décallage par rapport à la chute. N'a un sens que si la périodicité d'application est supérieure à 1 an.

#### 2.2.Processus de révision du TAC

- **Mécanisme de sortie** : Possibilité de révision du TAC au cours de la période pluri-annuelle conditionnellement à la condition suivante.
- **% de baisse provoquant la sortie** : Si 'Mécanisme de sortie' vaut oui, la révision du TAC est déclenchée par un % de baisse de cumulée calculée par rapport à l'indicateur d'abondance lors de la fixation du TAC. Noter que dans le cas d'un pourcentage à 0%, le TAC est révisé dès que l'indicateur baisse.
