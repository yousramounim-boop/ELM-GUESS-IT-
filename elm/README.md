# README - GUESS IT 

## Description générale
L’utilisateur doit **deviner un mot** à partir de ses **définitions et classes grammaticales**.  
Objectif : atteindre **20 points** pour gagner.

---

## 1. Modes de jeu et progression
Le jeu comporte **3 modes** :  

| Mode       | Points requis pour débloquer | Définitions visibles                                     |
|------------|-----------------------------|----------------------------------------------------------|
| Beginner   | 0                           | Toutes les définitions et classes grammaticales         |
| Medium     | 5                           | Une seule classe (celle avec le plus grand nombre de définitions) |
| Expert     | 10                          | Une seule définition d’une seule classe                |

- Déblocage automatique : le mode suivant est disponible dès que le score est atteint.  
- Les anciens modes ne sont plus accessibles pour le mot suivant.  
- Si le mot n’a qu’une classe et une définition, Medium applique la règle Expert.

---

## 2. Gestion des points et vies
- **Score** :
  - +1 pour chaque mot correct sans aide.
  - **Best score** toujours mis à jour si battu.
- **Vies** :
  - Début : 5 vies.
  - Mauvaise réponse → perte d’une vie.
  - Skip :
    - Retire 1 vie uniquement si aucune saisie.
    - Sinon, pas de perte de vie.
- Victoire ou game over → bloque saisie et skip jusqu’au replay.

---

## 3. Saisie et interactions
- **Input** : taper le mot, appuyer sur Enter pour valider.
  - Saisie vide → message : “Type something, then press Enter!”
  - Mauvaise réponse → perte d’une vie, feedback “Try again!”
  - Bonne réponse → score mis à jour, passage au mot suivant.
- **Skip** : passe au mot suivant. Retrait de vie si aucune saisie.
- **Show it** : révèle le mot, peut coûter une vie si utilisé pour tricher.

---

## 4. Feedback
- `Good` → Bonne réponse : “Right answer! Nice!”
- `Bad` → Mauvaise réponse ou skip : “Try again!” ou “Skipped!”
- `Info` → Info système : ex. “Game Over! Cannot skip.”

---

## 5. Conditions de victoire et fin de partie
- **Victoire** : score ≥ 20 → feedback “Victory! You reached 20 points!”  
- **Game Over** : vies = 0 → feedback “Game over! Final score: X — Best: Y”  
- **Replay** : réinitialise score = 0, vies = 5, **best score conservé**.

---

## 6. Logique des définitions visibles
- Beginner : toutes classes et définitions visibles.
- Medium : 1 classe visible (avec le plus grand nombre de définitions), toutes ses définitions.
- Expert : 1 classe, 1 définition.  
- Expert : si plusieurs définitions possibles, une seule est tirée au hasard.

---

## 7. Gestion du dictionnaire
- Les mots sont tirés aléatoirement depuis `Words.words`.
- Définitions/classes grammaticales via API :  
  `https://api.dictionaryapi.dev/api/v2/entries/en/<word>`
- Si l’API échoue → tirage automatique d’un nouveau mot.

---

## 8. Interface et UX
- **Top Bar** : score, best score, vies.  
- **Mode Selector** : boutons pour Beginner, Medium, Expert, affichage verrouillé/débloqué.  
- **Carte de mot** : mot et définitions visibles selon le mode.  
- **Actions** : boutons Skip et Show it.  
- **Input** : champ texte pour deviner le mot.

---

## 9. Récapitulatif des règles importantes
1. Best score toujours mis à jour si battu.  
2. Skip retire 1 vie uniquement si aucune réponse saisie.  
3. Mauvaise réponse retire 1 vie.  
4. Show it → révèle le mot, peut coûter une vie si utilisé pour tricher.  
5. Déblocage automatique des niveaux selon score.  
6. Beginner : toutes defs visibles. Medium : 1 classe max définitions. Expert : 1 classe, 1 def.  
7. Victoire à 20 points → replay possible avec vies réinitialisées.  
8. Erreurs de saisie ou API → feedback clair et tirage d’un nouveau mot.
# ELM-GUESS-IT-