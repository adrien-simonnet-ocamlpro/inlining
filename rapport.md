# Contexte

L'objectif du stage a été de proposer des heuristiques d'inlining pour le compilateur du langage OCaml. L'inlining consiste à injecter le corps d'une fonction en lieu et place d'un appel vers celle-ci dans l'objectif d'accélérer l'exécution du code (ou dans certains cas en diminuer sa taille). Néanmoins copier le corps d'une fonction peut faire augmenter la taille du code et conduire à de grosses pertes de performances lorsque certains seuils sont franchis. Vu la difficulté que serait de faire une analyse approfondie du meilleur choix d'inlining en fonction de tel ou tel processeur l'idée a été de se concentrer sur des heuristiques qui fonctionneront bien la plupart du temps. Cette optimisation est actuellement effectuée dans le compilateur natif par la série d'optimisations [flambda](https://v2.ocaml.org/manual/flambda.html), qui sera plus tard remplacé par [flambda2](https://github.com/ocaml-flambda/flambda-backend/tree/main/middle_end/flambda2) actuellement en développement. Découvrir et travailler sur un compilateur complexe comme celui d'OCaml n'a pas été jugé envisageable par mes tuteurs de stage, c'est la raison pour laquelle j'ai évolué sur un langage "jouet" qui n'est rien d'autre qu'une petite partie du noyeau fonctionnel d'OCaml. La première moitié du stage a donc consisté à implémenter les différentes phases de la compilation (langages intermédiaires et transformations) nécessaires pour mettre en place et tester lors de la seconde moitié du stage toutes les heuristiques d'inlining possibles qui me semblent pertinentes.

## Langage source

Dans le cadre du stage je n'ai traité que les fonctionnalités d'OCaml nécessaires pour aborder la plupart des cas intéressants de l'inlining et suffisantes pour obtenir un langage Turing-complet. En particulier comme je ne me suis concentré que sur le noyau fonctionnel d'OCaml, les objets, références, exceptions ou autres fonctionnalités du langage ont été ignorés.

### Lambda-calcul

Les [trois règles du lambda-calcul](https://fr.wikipedia.org/wiki/Lambda-calcul#Syntaxe) que sont les variables, l'application et l'abstraction sont évidemment des fonctionnalités indispensables.

### Opérations élémentaires

La manipulation des entiers et le branchement conditionnel sont incontournables pour réaliser des programmes dignes de ce nom.

### Fermetures (mutuellement) récursives

La prise en compte de la récursivité est fondamentale, premièrement en terme d'expressivité du langage (pour réaliser des tests poussés), deuxièmement il est intéressant de prendre en compte la complexité liée à l'impossibilité d'inliner tous les appels récursifs potentiels. La récursivité mutuelle permet d'ajouter une couche de complexité notamment lors de l'analyse.

### Types Somme et filtrage par motifs

La possibilité de construire des types Somme est une des fonctionnalités essentielles d'OCaml et permet de représenter quasiment n'importe quelle structure de données. De plus, de la même manière que le branchement conditionnel, le filtrage par motifs se prête particulièrement bien à l'inlining puisque connaître le motif peut permettre de filtrer de nombreuses branches et donc d'alléger un potentiel inlining.

## Langage utilisé

J'ai réalisé le compilateur en OCaml, en cohérence avec le langage source et les fondamentaux d'OCamlPro.

# Phases de compilation

Depuis le premier jour de stage j'ai considérablement augmenté le nombre de langages intermédiaires et transformations au fur et à mesure que la complexité des tâches à réaliser augmentait. J'ai fait des choix qui n'étaient pas toujours judicieux et dû faire machine arrière plusieurs fois ce qui m'a amené à me fixer les règles suivantes :

- Toujours, dans la mesure du raisonnable, réaliser les transformations en gardant sous la main les informations nécessaires à la transformation inverse (conserver par exemple la liste des alpha-conversions utiles pour le débogage) ;
- Ne pas réaliser plusieurs transformations à la fois, je pense qu'il est préférable d'avoir un langage intermédiaire en trop plutôt que d'avoir une transformation de l'un vers l'autre trop compliquée ;
- Éviter au mieux les constructions du langage qui ne sont pas utilisées lors d'une transformation mais utilisées dans la suivante, cas dans lequel il peut être pertinent de créer un langage intermédiaire.

Évidemment il est possible que ces règles que j'ai appliquées ne soient valables que dans le cadre d'un projet de petite taille.

## Analyse lexicale

L'analyse lexicale est la première étape de la compilation et convertit le programme source vu comme une chaîne de caractères en une liste de jetons.

### Lexique

Le lexique source est un sous-ensemble de celui d'[OCaml](https://v2.ocaml.org/releases/5.0/manual/lex.html) pour supporter les fonctionnalités qui m'intéressent.

- `(* *)` pour les commentaires
- `( )` pour le parenthésage
- `+ -` pour les opérations sur les entiers
- `if then else` pour le branchement conditionnel
- `fun ->` pour la création de fermeture
- `let rec = and in` pour les déclarations (mutuellement récursives)
- `type of | *` pour la déclaration d'un type somme
- `,` pour les constructeurs
- `match with | _` pour le filtrage par motif
- `['0'-'9']+` pour la déclaration d'un entier (positif)
- `['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*` pour désigner un constructeur
- `['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*` pour désigner une variable
- `[' ' '\t' '\n' '\r']` pour l'indentation et les sauts de ligne/retours chariot

### Analyseur lexical

Les jetons de l'analyse lexicale sont générés par [OCamllex](https://v2.ocaml.org/manual/lexyacc.html).

## Analyse syntaxique

L'analyse syntaxique est la seconde étape de la compilation et va convertir les jetons en un arbre de syntaxe abstraite. Les règles de syntaxe sont les mêmes que celles d'[OCaml](https://v2.ocaml.org/releases/5.0/manual/language.html) pour l'ensemble des jetons supportés.

### Arbre de syntaxe abstraite (AST)

#### Identificateurs

Le nom des variables et le nom des constructeurs sont des chaînes de caractères. Je ne me pose pas de question à savoir quel jeu de caractères OCaml est censé supporter.

$var \coloneqq string$

$typename \coloneqq string$

$tag \coloneqq string$

#### Types

Je ne réalise aucune vérification de typage, c'est pour cela que je traite les types comme de simples chaînes de caractères.

$string \coloneqq type$

#### Filtrage par motif

J'ai choisi de ne supporter que l'essentiel pour ce qui est du filtrage par motif. En particulier il n'est pas possible de déconstruire des termes à la volée ni de filtrer plusieurs motifs par la même expression.

$\text{Deconstructor} : tag \times var^{*} \rightarrow mp$

$\text{Joker} : var \rightarrow mp$

#### Opérateurs binaires

$\text{Add} : bop$

$\text{Sub} : bop$

#### Expressions

Les expressions constituent la base d'un langage fonctionnel.

$\text{Var} : var \rightarrow expr$

$\text{Fun} : var \times expr \rightarrow expr$

$\text{App} : expr \times expr \rightarrow expr$

$\text{Let} : var \times expr \times expr \rightarrow expr$

$\text{Letrec} : (var \times expr)^{\*} \times expr \rightarrow expr$

$\text{Int} : int \rightarrow expr$

$\text{Binary} : bop \times expr \times expr \rightarrow expr$

$\text{If} : expr \times expr \times expr \rightarrow expr$

$\text{Type} : typename \times (tag \times type)^{\*} \times expr \rightarrow expr$

$\text{Constructor} : tag \times expr^{\*} \rightarrow expr$

$\text{Match} : expr \times (mp \times expr)^{\*} \rightarrow expr$

### Analyseur syntaxique

L'AST est généré par [Menhir](https://ocaml.org/p/menhir/).

## Analyse sémantique

L'AST' est construit à partir de l'AST en résolvant les noms et les types (pour l'instant limités aux constructeurs). Je ne procède à aucune vérification de typage, cela n'étant pas le sujet principal de mon stage.

### Arbre de syntaxe abstraite' (AST')

#### Identificateurs

Le nom des variables et le nom des constructeurs deviennent des symboles représentés par des entiers.

$var \coloneqq int$

$tag \coloneqq int$

#### Opérateurs binaires

$\text{Add} : bop$

$\text{Sub} : bop$

#### Expressions

Les noms des constructeurs ont reçus un index relatif à leur position dans la déclaration du type qui sera par la suite utilisé dans les filtrages par motifs.

$\text{Var} : var \rightarrow expr$

$\text{Fun} : var \times expr \rightarrow expr$

$\text{App} : expr \times expr \rightarrow expr$

$\text{Let} : var \times expr \times expr \rightarrow expr$

$\text{Letrec} : (var \times expr)^{*} \times expr \rightarrow expr$

$\text{Int} : int \rightarrow expr$

$\text{Binary} : bop \times expr \times expr \rightarrow expr$

$\text{If} : expr \times expr \times expr \rightarrow expr$

$\text{Constructor} : tag \times (expr^{*}) \rightarrow expr$

$\text{Match} : expr \times (tag \times var^{\*} \times expr)^{\*} \times expr \rightarrow expr$

### Analyseur sémantique

Toutes les variables sont alpha-converties et retournées à part (la liste des substitutions ne fait pas partie du AST'). Les variables libres dans le programme sont autorisées, également alpha-converties et retournée à part. L'acceptation de variables libres dans le programme permet à mes yeux de faciliter la gestion du non-déterminisme et d'éviter toute ambiguïté lors de l'analyse. En effet les entrées-sorties peuvent être vues comme des variables libres qui ne sont connues qu'au moment de l'exécution du programme, ce qui permet de s'assurer par exemple qu'un affichage sur la sortie ne serait pas optimisé (de la même manière je me pose la question à savoir si la mémoire, dans le cas où je traiterais les effets de bord, peut être modélisée comme une variable libre, ce qui expliciterait le non-déterminisme des effets de bord).

$$
   \begin{align}
      \tag{Int}
      \over \left( \text{Int} ~ i \right) ~ S ~ C \vdash_{\text{ast'}} \left( \text{Int} ~ i \right) ~ \emptyset ~ \emptyset
   \end{align} $$

$$
   \begin{align}
      \tag{Binary}
      \begin{split}
         e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         e_2 ~ \left( A \cup V_{e_1} \right) ~ C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ V_{e_2}
      \end{split}
      \over \left( \text{Binary} ~ \diamond ~ e_1 ~ e_2 \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Binary} ~ \diamond ~ e_1' ~ e_2' \right) ~ \left( S_{e_1} \cup S_{e_2} \right) ~ \left( V_{e_1} \cup V_{e_1} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Fun}
      \begin{split}
         e ~ \left( A \cup \lbrace x = id_x \rbrace \right) ~ C &\vdash_{\text{ast'}} e' ~ S_{e} ~ V_{e}
      \end{split}
      \over \left( \text{Fun} ~ x ~ e \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Fun} ~ id_x ~ e' \right) ~ \left( S_{e} \cup \lbrace id_x = x \rbrace \right) ~ V_{e}
   \end{align} $$

$$
   \begin{align}
      \tag{Var1}
      \begin{split}
         x \in D(A)
      \end{split}
      \over \left( \text{Var} ~ x \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Var} ~ A\left( x \right) \right) ~ \emptyset ~ \emptyset
   \end{align} $$

$$
   \begin{align}
      \tag{Var2}
      \begin{split}
         x \notin D(A)
      \end{split}
      \over \left( \text{Var} ~ x \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Var} ~ id_x \right) ~ \emptyset ~ \lbrace x = id_x \rbrace
   \end{align} $$

$$
   \begin{align}
      \tag{Let}
      \begin{split}
         e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         e_2 ~ \left( A \cup V_{e_1} \cup \lbrace x = id_x \rbrace \right) ~ C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ V_{e_2}
      \end{split}
      \over \left( \text{Let} ~ x ~ e_1 ~ e_2 \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Let} ~ id_x ~ e_1' ~ e_2' \right) ~ \left( S_{e_1} \cup S_{e_2} \cup \lbrace id_x = x \rbrace \right) ~ \left( V_{e_1} \cup V_{e_1} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{If}
      \begin{split}
         e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         e_2 ~ \left( A \cup V_{e_1} \right) ~ C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ V_{e_2} \\
         e_3 ~ \left( A \cup V_{e_1} \cup V_{e_2} \right) ~ C &\vdash_{\text{ast'}} e_3' ~ S_{e_3} ~ V_{e_3}
      \end{split}
      \over \left( \text{If} ~ e_1 ~ e_2 ~ e_3 \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Binary} ~ e_1' ~ e_2' ~ e_3' \right) ~ \left( S_{e_1} \cup S_{e_2} \cup S_{e_3} \right) ~ \left( V_{e_1} \cup V_{e_1} \cup V_{e_3} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{App}
      \begin{split}
         e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         e_2 ~ \left( A \cup V_{e_1} \right) ~ C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ V_{e_2}
      \end{split}
      \over \left( \text{App} ~ e_1 ~ e_2 \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{App} ~ e_1' ~ e_2' \right) ~ \left( S_{e_1} \cup S_{e_2} \right) ~ \left( V_{e_1} \cup V_{e_1} \right)
   \end{align} $$



$$
   \begin{align}
      \tag{Type}
      \begin{split}
         e ~ A ~ \left( C \cup \lbrace v_i = i, \forall i \[|1, n\|] \rbrace \right) &\vdash_{\text{ast'}} e' ~ S_{e} ~ V_{e}
      \end{split}
      \over \left( \text{Type} ~ s ~ \left( v_i \right)^{i=1 \dots i=n} ~ e \right) ~ A ~ C \vdash_{\text{ast'}} e' ~ S_{e} ~ V_{e}
   \end{align} $$
   
$$
   \begin{align}
      \tag{Constructor}
      \begin{split}
         e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         \dots \\
         e_n ~ \left( \bigcup_{i=1}^{n-1} S_{e_{i-1}} \cup A \right) ~ C &\vdash_{\text{ast'}} e_n' ~ S_{e_n} ~ V_{e_n}
      \end{split}
      \over \left( \text{Constructor} ~ s ~ \left( e_i \right)^{i=1 \dots n} \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Constructor} ~ C\[s\] ~ \left( e_i' \right)^{i=1 \dots n} \right) ~ \left( \bigcup_{i=1}^{n} S_{e_i} \right) ~ \left( \bigcup_{i=1}^{n} V_{e_i} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Letrec}
      \begin{split}
         e_1 ~ \bigcup_{i=1}^{n-1} \lbrace x_i = id_{x_i} \rbrace \cup A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         \dots \\
         e_n ~ \left( \bigcup_{i=1}^{n-1} \left( S_{e_{i-1}} \cup \lbrace x_i = id_{x_i} \rbrace \right) \cup A \right) ~ C &\vdash_{\text{ast'}} e_n' ~ S_{e_n} ~ V_{e_n}
      \end{split}
      \over \left( \text{Letrec} ~ \left( x_i, e_i \right)^{i=1 \dots n} ~ e \right) ~ A ~ C \vdash_{\text{ast'}} \left( \text{Letrec} ~ \left( id_{x_i}, e_i' \right)^{i=1 \dots n} ~ e' \right) ~ \left( \bigcup_{i=1}^{n} S_{e_i} \right) ~ \left( \bigcup_{i=1}^{n} V_{e_i} \right)
   \end{align} $$




## CFG

La conversion CFG transforme l'AST' en un ensemble (non ordonné) de basic blocs (qui n'est pas à proprement parler un CFG). L'idée est de perdre le moins d'informations possible du programme source tout en ayant sous la main un langage intermédiaire qui permette une analyse simple et puissante.

### Graphe de flôt de contrôle

#### Identificateurs

$var \coloneqq int$

$pointer \coloneqq int$

$tag \coloneqq int$

$frame \coloneqq pointer \times var^{*}$

#### Instructions élémentaires

$\text{Prim} : prim \times var^{*} \rightarrow instruction$

$\text{Var} : var \rightarrow instruction$

$\text{Tuple} : var^{*} \rightarrow instruction$

$\text{Get} : var \times int \rightarrow instruction$

$\text{Closure} : pointer \times var^{*} \rightarrow instruction$

$\text{Constructor} : tag \times var^{*} \rightarrow instruction$

#### Expression

Une instruction est soit une déclaration soit un branchement. Une déclaration construit une valeur et l'associe à un identifiant unique. Les valeurs ne peuvent être construites qu'à partir de constantes ou identifiants. Un branchement représente le transfert d'un basic block à un autre que ce soit par le biais d'un appel (fermeture), d'un retour de fonction (return) ou d'un saut conditionnel (filtrage par motif).

> `CallDirect` est un branchement qui n'est pas atteignable depuis le programme source et n'apparaît qu'après une étape d'analyse.

$\text{Let} : var \times instruction \times expr \rightarrow expr$

$\text{ApplyBlock} : pointer \times var^{*} \rightarrow expr$

$\text{CallDirect} : pointer \times var \times var^{*} \times frame \rightarrow expr$

$\text{Call} : var \times var^{*} \times frame \rightarrow expr$

$\text{If} : var \times (pointer \times var^{\*}) \times (pointer \times var^{\*}) \times var^{\*} \rightarrow expr$

$\text{MatchPattern} : var \times (tag \times var^{\*} \times pointer \times var^{\*})^{\*} \times (pointer \times var^{\*}) \times var^{\*} \rightarrow expr$

$\text{Return} : var \rightarrow expr$

$\text{IfReturn} : pointer \times var \times var^{*} \rightarrow expr$

$\text{MatchReturn} : pointer \times var \times var^{*} \rightarrow expr$

#### Bloc

Chaque bloc est construit différemment en fonction de l'expression à partir de laquelle il est construit (fermeture, retour de fonction ou saut conditionnel), est clos (il explicite les arguments dont il a besoin) et contient une suite de déclarations de variables suivies d'une instruction de branchement.

$\text{Cont} : var^{\*} \rightarrow block$

$\text{Clos} : var^{\*} \times var^{\*} \rightarrow block$

$\text{Return} : var \times var^{\*} \rightarrow block$

$\text{IfBranch} : var^{\*} \times var^{\*} \rightarrow block$

$\text{IfJoin} : var \times var^{*} \rightarrow block$

$\text{MatchBranch} : var^{\*} \times var^{\*} \times var^{\*} \rightarrow block$

$\text{MatchJoin} : var \times var^{\*} \rightarrow block$

#### Blocs

$blocks \coloneqq pointer \rightarrow (block \times expr)$

### Algorithme de génération du CFG

L'algorithme a la signature suivante : $cfg : expr_{ast'} \times var_{cfg} \times var_{cfg}^{\*} \times expr_{cfg} \rightarrow expr_cfg \times var_{cfg}^{\*} \times blocks$.

- Le premier argument, noté $e$, correspond à l'expression sous forme d'AST' à intégrer au CFG.
- Le deuxième argument, noté $\sigma$, correspond au nom de la variable dans lequel devrait être conservé le résultat de l'évaluation de $e$.
- Le troisième argument, noté $\Sigma$, correspond aux variables devant être sauvegardées durant l'évaluation de $e$ pour être réstaurées après (ne doit pas contenir $\sigma$).
- Le quatrième argument, noté $\epsilon$, correspond à l'expression déjà transpilée au format CFG qui sera éxécutée après $e$. Elle doit faire usage de $\sigma$ et chaque variable libre qui y apparaît doit figurer dans $\Sigma$.
- Le premier résultat, noté $\epsilon'$, correspond à $e$ transpilée.
- Le second résultat, noté $\Sigma'$, correspond aux variables libres apparaissant dans $e$ (en théorie, les variables libres de $\epsilon'$ sont exactement $\Sigma \cup \Sigma'$).
- Le troisième résultat, noté $\beta$, est l'ensemble des blocs générés par la transpilation de $e$.

$$
   \begin{align}
      \tag{Int}
      \over \left( \text{Int} ~ i \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \left( \sigma = \text{Int} ~ i; \epsilon \right) ~ \emptyset ~ \emptyset            \end{align} $$

$$
   \begin{align}
      \tag{Var}
      \over \left( \text{Var} ~ v \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \left( \sigma = v; \epsilon \right) ~ \lbrace v \rbrace ~ \emptyset
   \end{align} $$

$$
   \begin{align}
      \tag{Let}
      \begin{split}
         e_2 ~ \sigma ~ \Sigma ~ \epsilon &\vdash_{\text{cfg}} \epsilon_{e_2} ~ \Sigma_{e_2} ~ \beta_{e_2} \quad \Sigma_3 = \Sigma_{e_2} \setminus \lbrace v \rbrace \\
         e_1 ~ v ~ \left( \Sigma \cup \Sigma_3 \right) ~ \epsilon_{e_2} &\vdash_{\text{cfg}} \epsilon_{e_1} ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split}
      \over \left( \text{Let} ~ v ~ e_1 ~ e_2 \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_{e_1} ~ \left( \Sigma_3 \cup \Sigma_{e_1} \right) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \right)
   \end{align} $$

$$
   \begin{align}
   \tag{Binary}
      \begin{split}
         e_2 ~ \sigma_{e_2} ~ \left( \Sigma \cup \lbrace \sigma_1 \rbrace \right) ~ (\sigma = \sigma_{e_1} \diamond \sigma_{e_2}; \epsilon) &\vdash_{\text{cfg}} \epsilon_{e_2} ~ \Sigma_{e_2} ~ \beta_{e_2} \\
         e_1 ~ \sigma_{e_1} ~ (\Sigma_{e_2} \cup \Sigma) ~ \epsilon_{e_2} &\vdash_{\text{cfg}} \epsilon_{e_1} ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split}
      \over \left( \text{Binary} ~ \diamond ~ e_1 ~ e_2 \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_{e_1} ~ \left( \Sigma_{e_1} \cup \Sigma_{e_2} \right) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Fun}
      \begin{split}
         e ~ \sigma_{e} ~ \emptyset ~ \left( \text{Return} ~ \sigma_{e} \right) \vdash_{\text{cfg}} \epsilon_{e} ~ \Sigma_{e} ~ \beta_{e} \quad \Sigma_2 = \Sigma_{e} \setminus \lbrace \sigma_a \rbrace
      \end{split}
      \over \left( \text{Fun} ~ \sigma_a ~ e \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \left( \sigma = \text{Closure} ~ \rho ~ \Sigma_2; \epsilon \right) ~ \Sigma_2 ~ \left( \beta_{e} \sqcup \lbrace \rho = \text{Clos} ~ \Sigma_2 ~ \left( \sigma_a \right) ~ \epsilon_{e} \rbrace \right)
   \end{align} $$

$$
   \begin{align}
      \tag{If}
      \begin{split}
         e_2 ~ \sigma_{e_2} ~ \Sigma ~ (\text{Ifreturn} ~ \rho_{e_1} ~ \sigma_{e_2} ~ \Sigma) &\vdash_{\text{cfg}} \epsilon_{e_2} ~ \Sigma_{e_2} ~ \beta_{e_2} \\
         e_3 ~ \sigma_{e_3} ~ \Sigma ~ (\text{Ifreturn} ~ \rho_{e_1} ~ \sigma_{e_3} ~ \Sigma) &\vdash_{\text{cfg}} \epsilon_{e_3} ~ \Sigma_{e_3} ~ \beta_{e_3} \\
         e_1 ~ \sigma_{e_1} ~ (\Sigma \cup \Sigma_{e_2} \cup \Sigma_{e_3}) ~ (\text{If} ~ \sigma_{e_1} ~ \rho_{e_2} ~ \Sigma_{e_2} ~ \rho_{e_3} ~ \Sigma_{e_3} ~ \Sigma) &\vdash_{\text{cfg}} \epsilon_{e_1} ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split} \\
      \beta_{e_1e_2e_3} = \lbrace \rho_{e_1} = \text{Ifjoin} ~ \sigma ~ \Sigma ~ \epsilon, \rho_{e_2} = \text{Ifbranch} ~ \sigma_{e_2} ~ \Sigma ~ \epsilon_{e_2}, \rho_{e_3} = \text{Ifbranch} ~ \sigma_{e_3} ~ \Sigma ~ \epsilon_{e_3} \rbrace
      \over (\text{If} ~ e_1 ~ e_2 ~ e_3) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_{e_1} ~ (\Sigma_{e_1} \cup \Sigma_{e_2} \cup \Sigma_{e_3}) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \sqcup \beta_{e_3} \sqcup \beta_{e_1e_2e_3} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{App}
      \begin{split}
         e_2 ~ \sigma_{e_2} ~ \left( \Sigma \cup \lbrace \sigma_{e_1} \rbrace \right) ~ \left( \text{Call} ~ \sigma_{e_1} ~ \left( \sigma_{e_2} \right) ~ \rho ~ \Sigma \right) &\vdash_{\text{cfg}} \epsilon_2 ~ \Sigma_{e_2} ~ \beta_{e_2} \\
         e_1 ~ \sigma_{e_1} ~ \left( \Sigma_{e_2} \cup \Sigma \right) ~ \epsilon_2 &\vdash_{\text{cfg}} \epsilon_1 ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split}
      \over \left( \text{App} ~ e_1 ~ e_2 \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_2 ~ \left( \Sigma_{e_1} \cup \Sigma_{e_2} \right) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \sqcup \lbrace \rho = \text{Return} ~ \sigma ~ \Sigma ~ \epsilon \rbrace \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Constructor} f = \begin{cases} f_0 =
      (\text{Constructor} ~ t ~ \alpha) ~ \Sigma ~ \beta \\
      f_n = {f_{n-1} = \epsilon_{n-1} ~ \Sigma_{n-1} ~ \beta_{n-1}
         \quad a_n ~ \alpha_n ~ \alpha \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon_n ~ \Sigma_n ~ \beta_n
         \over \epsilon_n ~ \Sigma_n \cup \Sigma_{n-1} \setminus \lbrace \alpha_n \rbrace ~ \beta_n \cup \beta_{n-1}}
      \end{cases}
      \over (\text{Constructor} ~ t ~ a_n) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} f_n
   \end{align} $$




$$ \begin{align} \begin{cases}
      \epsilon_0 = (\sigma = \text{Constructor} ~ t ~ (\overline{a_1} \dots \overline{a_n}); \epsilon) \\
      \epsilon_n =
         { a_n ~ \overline{a_n} ~ \Sigma \cup \Sigma_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \epsilon }
   \end{cases}
   \begin{cases}
      \Sigma_0 = \lbrace \overline{a_1}, \dots, \overline{a_n} \rbrace \\
      \Sigma_n =
         { a_n ~ \overline{a_n} ~ \Sigma \cup \Sigma_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \Sigma \cup \Sigma_{n-1} \setminus \lbrace \overline{a_n} \rbrace }
   \end{cases}
   \begin{cases}
      \beta_0 = \emptyset \\
      \beta_n =
         { a_n ~ \overline{a_n} ~ \Sigma \cup \Sigma_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \beta \cup \beta_{n-1} }
   \end{cases}
   \over (\text{Constructor} ~ t ~ (a_1 \dots a_n)) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_n ~ \Sigma_n ~ \beta_n \end{align} $$

$$
   \begin{align}
      \tag{Match}
      \begin{split}
         d ~ \overline{d} ~ \Sigma ~ (\text{Matchreturn} ~ \rho_\epsilon ~ \overline{d} ~ \Sigma) \vdash_{\text{cfg}} \epsilon_d ~ \Sigma_d ~ \beta_d \\
         \begin{cases}
            \beta_0 = \emptyset \\
             \beta_n =
            { e_n ~ \overline{e_n} ~ \Sigma ~ (\text{Matchreturn} ~ \rho_\epsilon ~ \overline{e_n} ~ \Sigma) \vdash_{\text{cfg}} \epsilon ~ \Sigma_{e_n} ~ \beta
            \quad \Sigma_{e_n} = \Sigma_{e_n} \setminus \lbrace a_n^1, \dots, a_n^{m_n} \rbrace
            \over \beta \cup \beta_{n-1} \cup \lbrace \langle \rho_{e_n}, \text{Matchbranch} ~ \left( a_n^1 \dots a_n^{m_n} \right) ~ \Sigma_{e_n} ~ \Sigma, \epsilon \rangle \rbrace }
         \end{cases} \\
         { \Sigma_{e_n} =
         e_n ~ \overline{e_n} ~ \Sigma ~ (\text{Matchreturn} ~ \rho_\epsilon ~ \overline{e_n} ~ \Sigma) \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \Sigma \setminus \lbrace a_n^1, \dots, a_n^{m_n} \rbrace } \\
         e ~ \sigma_e ~ \left( \bigcup_{i=1}^{n} \Sigma_{e_i} \cup \Sigma_d \cup \Sigma \right) ~ \left( \text{Matchpattern} ~ \sigma_e ~ \left( \langle t_i, \rho_{e_i}, \left( a_i^j \right)^{j=1 \dots m_i}, \Sigma_{e_i} \rangle \right)^{i=1 \dots n} ~ \langle \rho_d, \Sigma_d \rangle ~ \Sigma \right) \vdash_{\text{cfg}} \epsilon_1 ~ \Sigma_1 ~ \beta_1
      \end{split}
      \over (\text{Match} ~ e ~ \left( \langle t_i, \left( a_i^j \right)^{j=1 \dots m_i}, e_i \rangle \right)^{i=1 \dots n} ~ d) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_2 ~ (\Sigma_1 \cup \Sigma_2) ~ (\beta_1 \cup \beta_2)\[\rho = \text{Return} ~ \sigma ~ \Sigma ~ \epsilon\]
   \end{align} $$


## Analyse

L'analyse est l'étape la plus compliquée et probablement la plus importante pour permettre d'inliner de manière efficace. L'objectif principal est de transformer le meieux possible les sauts indirects (appels de fonctions) en sauts directs afin d'être capable d'inliner de tels sauts. Ensuite, même si ce n'est pas obligatoire, il est intéressant de disposer d'une analyse des valeurs assez précise pour se faire une idée de quand inliner pour obtenir les meilleurs bénéfices. Cette analyse s'effectue au niveau du CFG afin d'exploiter la sémantique du langage (et donc de conserver certaines relations) tout en disposant des informations nécessaires sur les blocs. Sur conseil de mon tuteur, je réalise une analyse par zone d'allocation, c'est à dire que les paramètres des blocs sont identifiés par un ensemble de points d'allocation et chaque point d'allocation contient une valeur abstraite. Etant donné que chaque valeur créée est déclarée (il n'existe pas de valeur temporaire) avec un nom de variable unique (garanti par l'alpha-conversion), un point d'allocation peut donc être identifé par un nom de variable. Ce choix donne des garanties de terminaison (il existe un nombre fini de points d'allocation dans le programme) tout en permettant une analyse poussée qui autorise par exemple la récursivité lors de la construction des blocs (fondamental pour traiter les listes).

### Domaines



#### Entiers

J'ai choisi de représenter les entiers de la manière la plus simple qui soit, c'est à dire des singletons munis de Top ($Z$).

$\text{Top} : int_d$

$\text{Singleton} : int \rightarrow int_d$

L'union de deux entiers donne toujours Top sauf lorsqu'il s'agit de deux singletons de même valeur.

$u = v \Rightarrow \lbrace u \rbrace \cup_{int_d} \lbrace v \rbrace = \lbrace u \rbrace$

$x \cup_{int_d} y = Top$

Le domaine pour les fermetures (resp. les constructeurs) est un environnement d'identifiant vers contexte, où l'identifiant correspond au pointeur de fonction (resp. au tag), et le contexte correspond aux variables libres (resp. au payload). Étant donné que les pointeurs de fonctions, les tags ainsi que les contextes (ensemble de zones d'allocations) sont des ensembles bornés, l'union de deux abstractions est garantie de converger.

$closure_d \coloneqq pointer \rightarrow \mathcal{P}(var)^{\*}$

$constructor_d \coloneqq tag \rightarrow \mathcal{P}(var)^{\*}$

En pratique, $pointer = tag = int$, c'est pour cela que j'utilise le même domaine pour les constructeurs et les tags.

L'union de deux domaines de fermetures consiste à conserver les entrées distinctes et d'unir les valeurs des entrées communes.

> Deux entrées communes sont censées avoir le même nombre de valeurs.

$$x \cup_{closure} y = \forall z \in \mathcal{D}(x) \cup \mathcal{D}(y),
   \begin{cases}
      \left( x(z)\_i \cup y(z)\_i \right)_{i=1}^{i=n} \text{ si } z \in \mathcal{D}(x) \text{ et } z \in \mathcal{D}(y) \text{ et } |x(z)| = |y(z)| = n \\
      x(z) \text{ si } z \in \mathcal{D}(x) \\
      y(z) \text{ si } z \in \mathcal{D}(y)
   \end{cases}$$

Une valeur abstraite est soit un entier soit une fermeture.

$\text{IntDomain} : int_d \rightarrow value_domain$

$\text{ClosureDomain} : closure_d \rightarrow value_domain$

Comme chaque valeur est toujours initialisée et déclarée avec un identifiant unique, elles sont conservées dans une table d'association correspondant aux allocations.

$allocations \coloneqq var \rightarrow value_domain$


Lors de l'analyse, les blocs conserveront désormais un ensemble de points d'allocations pour chacune de ses variables.

$cont_type \coloneqq \text{block}\[var/\mathcal{P}(var)\]$

Une frame correspond à un étage de la pile, c'est à dire le pointer vers un bloc qui sera éxécuté au prochain retour d'appel avec les paramètres qui ont été sauvegardés.

$frame \coloneqq pointer \times (\mathcal{P}(var))^{\*}$

La pile d'appel est une liste ordonnée d'appels.

$stack_allocs \coloneqq frame^{\*}$

Un bloc à analyser est ientifié par son pointeur, ses arguments, la pile d'appel et enfin au contexte d'allocations du moment où il est appelé.

$bbloc \coloneqq pointer \times cont_type \times stack_allocs \times allocations$

Une fonction qui réduit la taille de la pile.

$stack_reduce \coloneqq stack_allocs \rightarrow stack_allocs$

Le contexte d'appel contient la pile d'appel et les arguments d'appel.

> Il est important de noter que si l'on suppose la pile d'appel comme ayant une taille finie (garantie après réduction), la dimension d'un contexte est bornée, ce qui est essentiel pour garantir la terminaison.

$context \coloneqq stack_allocs \times cont_type$

Une table de contextes d'allocations associe un contexte à l'état de ses allocations.

> La dimension d'un contexte étant bornée, les contextes sont donc dénombrables (on peut donc implémenter la table d'association correspondante).

$alloccontexte \coloneqq context \rightarrow allocations$

Une table de contextes de blocs associe chaque bloc à ses contextes d'allocation.

$bloccontexte \coloneqq pointer \rightarrow alloccontexte$

Un appel analysé est une paire associant les paramètres passés avec leurs allocations correspondantes.

$callanalysed \coloneqq cont_type \times allocations$

Un programme analyse (càd le résultat de l'analyse) est une table associant chaque bloc à son appel analysé.

$analysis \coloneqq pointer \rightarrow callanalysed$

### Algorithme d'analyse

$\text{analysis} : bbloc^{\*} \times stack_reduce \times blocks \times bloccontexte \rightarrow analysis$




```ocaml
let rec analysis (conts: (int * cont_type * stack_allocs * allocations) list) (reduce: stack_allocs -> stack_allocs) (prog: cont) (map: (allocations ContextMap.t) Cps.PointerMap.t) : (allocations * cont_type) Cps.PointerMap.t =
  match conts with
  | [] -> Cps.PointerMap.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, new_env), allocations) -> join_allocations allocs allocations, join_blocks acc new_env) (let ((_, new_env), allocations) = List.hd (ContextMap.bindings contexts) in allocations, new_env) (List.tl (ContextMap.bindings contexts))) map
  | (k, block', stack''', allocations) :: conts' -> begin
    Logger.start "k%d %a Stack: %a Allocs: %a\n" k pp_block  block' pp_stack stack''' pp_allocations allocations;
    Logger.stop ();

      let stack = reduce stack''' in

      (* Already seen this block. *)
      if Cps.PointerMap.mem k map then begin
        let old_contexts = Cps.PointerMap.find k map in
        
        (* Already seen this context. *)
        if ContextMap.mem (stack, block') old_contexts then begin
          let old_allocations = ContextMap.find (stack, block') old_contexts in
          let new_allocations = join_allocations old_allocations allocations in
          (* Already seen these allocations. *)
          if Cps.VarMap.equal value_cmp new_allocations old_allocations then begin
            match stack''' with
            | [] -> analysis conts' reduce prog map
            | (k', args) :: _stack' -> analysis ((k', Return (Cps.VarSet.empty, args), _stack', new_allocations) :: conts') reduce prog map
          end else begin
            let block, expr = Cps.PointerMap.find k prog in
            let next_conts = analysis_cont expr stack''' (block_env block block') new_allocations in
            analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (ContextMap.add (stack, block') new_allocations old_contexts) map)
          end
        end else begin
          let block, expr = Cps.PointerMap.find k prog in
          let next_conts = analysis_cont expr stack''' (block_env block block') allocations in
          analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (ContextMap.add (stack, block') allocations old_contexts) map)
        end
      end else begin
        let block, expr = Cps.PointerMap.find k prog in
        let next_conts = analysis_cont expr stack''' (block_env block block') allocations in
        analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (ContextMap.singleton (stack, block') allocations) map)
      end
    end
```


### Abstractions

Actuellement j'utilise deux abstractions pour représenter toutes les valeurs du langage. La première pour les entiers qui est simplement le domaine singleton. La deuxième pour les fermetures (resp. les constructeurs) est un environnement d'identifiant vers contexte, où l'identifiant correspond au pointeur de fonction (resp. au tag), et le contexte correspond aux variables libres (resp. au payload). Étant donné que les pointeurs de fonctions, les tags ainsi que les contextes (ensemble de zones d'allocations) sont des ensembles bornés, l'union de deux abstractions est garantie de converger.

### Preuve de terminaison

Les noms de variables, par extension les zones d'allocations, étant en nombre fini dans le programme, l'ensemble identifiant les valeurs est également fini. De la même manière, un contexte d'appel (ensemble fini de valeurs correspondant aux arguments et pointeur de bloc), est un ensemble fini étant donné que le nombre de blocs dans le programme est également borné. Reste la question épineuse de comment garantir que la pile d'appels ne croît pas infiniment. Afin de tenter d'obtenir une précision maximale, je détecte d'éventuels motifs sur la pile en regardant si 1..N contextes d'appels se répètent et le cas échéant je supprime la répétition. Par exemple la pile d'appels A::B::A::B::C::[] sera remplacée par A::B::C::[]. Après avoir implémenté cette méthode, mes tuteurs m'ont rapidement fait comprendre qu'elle ne pouvait pas garantir la terminaison. Pour la suite du stage je vais certainement devoir durcir la détection de motifs en passant à 0-CFA ou 1-CFA.

## CFG concret

Le CFG concret est construit à partir du CFG en concretisant quasiment tous les traits de langage propres à OCaml. A chaque construction de valeur du langage OCaml est associée une structure de données, la plupart d'entre elles devenant des n-uplets. Tous les types de blocs fusionnent en un seul en fixant la sémantique des sauts (passage de l'environnement comme argument) et chaque type de branchement est transformé en un saut (direct ou indirect) avec la possibilité d'ajouter des contextes d'appel sur la pile (seule l'instruction d'appel ajoute un contexte lors de cette transformation).

### Inlining

L'inlining a lieu sur le CFG concret car les modifications apportées peuvent casser la sémantique d'appel ce qui doit être représenté au niveau de la pile. Inliner un bloc consiste à intégrer son contenu dans le bloc appelant à la place de la dernière instruction (branchement). Chaque argument du bloc inliné est remplacé par la variable qui lui a été assignée lors du branchement par le bloc appelant. Pour l'instant seuls les appels directs peuvent être inlinés. Si lors de l'appel des contextes étaient empilés sur la pile, alors le branchement du bloc inliné en tiendra compte. En particulier, si le branchement du bloc inliné est un retour de fonction celui-ci dépilera la pile et deviendra un saut direct. Dans les autres cas les contextes du bloc appelant sont empilés sur les contextes du bloc appelé, ce qui permet d'avoir des sauts vers l'intérieur d'une fonction.

### Interprétation

C'est le CFG concret que j'interprête pour dans un premier temps m'assurer de la validité de toutes les transformations. Pour la suite du stage je serai amené à extraire de nombreuses informations issues de l'interprétation pour vérifier la pertinance des heuristiques mises en place.

# Heuristiques d'inlining

Les phases de compilation étant prêtes et à mes yeux suffisamment expressives pour permettre toute forme d'inlining, les heuristiques en tant que telles seront traitées lors de la seconde moitié du stage. De nombreux tests complets seront à réaliser pour à la fois vérifier la pertinence des heuristiques implémentées mais également en détecter de nouvelles.

## Inlining partiel

Une heuristique possible sur laquelle je vais me pencher sera l'[inlining partiel](https://developers.redhat.com/blog/2014/10/29/rhel7-gcc-optimizations-partial-inlining), c'est à dire potentiellement inliner seulement les premiers blocs d'une fonction. La manière dont je représente le programme me permet de sauter à l'intérieur d'une fonction, ce qui n'est actuellement pas possible avec flambda2. La question est donc de savoir s'il existe des cas où cette heuristique pourrait être intéressante, que ce soit à la fois en terme d'optimisations de la taille ou du temps d’exécution. Une première idée de situations intéressantes qui me vient à l'esprit est le (très) grand nombre de fonctions en OCaml qui filtrent en début de fonction un de leurs arguments ce qui, à ma connaissance, se représente une fois compilé comme quelques instructions suivies d'un saut. Le filtrage en lui même n'est pas une opération spécialement coûteuse en terme d'espace, on transformerait ici un appel de fonction en un filtrage vers des blocs à "l'intérieur" de celle-ci (avec évidemment les opérations sur la pile qu'il convient de faire comme un appel classique). De plus l'inliner peut permettre de gagner en informations sur le motif, ce qui peut rendre possible de transformer le filtrage en appel direct, les gains seraient considérables.

Un exemple qui me pousse à croire qu'une telle heuristique peut être prometteuse est fourni sur le site de [flambda](https://v2.ocaml.org/manual/flambda.html#ss:flambda-inlining-overview) :

```ocaml
let f b x =
  if b then x else ... big expression ...

let g x = f true x
```

En appliquant les transformations pour faire apparaître les blocs, les branchements et les manipulations de la pile on obtient ce pseudo-code sous forme de basic blocks (sans distinguer les appels directs des appels indirects) :

```ocaml
let rec f b x stack =
  if b then f0 x stack else f1 stack
and f0 x stack =
  f2 x stack
and f1 stack =
  f2 (... big expression ...) stack
and f2 x (p::stack) =
  p x stack
and g x stack =
  f true x (g0::stack)
and g0 x (p::stack) =
  p x stack
```

Dans le cas présent la fonction `f` peut être vue comme un bloc d'une seule instruction. L'idée est donc d'inliner systématiquement ce bloc (pas la fonction complète, d'où la notion d'inlining partiel), et on obtient le code suivant où l'appel vers `f` est inliné dans `g` :

```ocaml
let rec f b x stack =
  if b then f0 x stack else f1 stack
and f0 x stack =
  f2 x stack
and f1 stack =
  f2 (... big expression ...) stack
and f2 x (p::stack) =
  p x stack
and g x stack =
  if true then f0 x (g0::stack) else f1 (g0::stack)
and g0 x (p::stack) =
  p x stack
```

Cet inlining n'est possible que si toutes les branches du `if` empilent les mêmes contextes sur la pile, ce qui est normalement toujours le cas quand le code est bien construit. Avec cet inlining, indépendamment de la valeur de `b`, on économise un saut et on se donne davantage d'informations sur le contexte dans le bloc inliné qui pourraient être utiles à d'éventuelles simplifications. Après simplifications, en supposant que le premier bloc de `f` est systématiquement inliné pour chaque appel et en inlinant les appels terminaux, on obtient le code suivant :

```ocaml
let rec f0 x (p::stack) =
  p x stack
and f1 (p::stack) =
  p (... big expression ...) stack
and g x (p::stack) =
  p x stack
```

Évidemment au stade actuel cette heuristique ne reste qu'une hypothèse et il faudra vérifier à la fois sa faisabilité et son efficacité. Il est par ailleurs fort probable qu'elle ne soit pas possible à mettre en place dans l'évaluateur de bytecode.
