# Contexte

L'objectif du stage est de proposer des idées d'heuristiques d'inlining pour le compilateur d'OCaml. L'inlining consiste à injecter le corps d'une fonction en lieu et place d'un appel vers celle-ci dans l'objectif d'accélérer l'exécution du code (ou dans certains cas d'en diminuer sa taille). Néanmoins copier le corps d'une fonction peut faire augmenter la taille du code et conduire à de grosses pertes de performances lorsque certains seuils sont franchis. Vu la difficulté que serait de faire une analyse approfondie du meilleur choix d'inlining en fonction de tel ou tel processeur l'idée est de se concentrer sur des heuristiques qui fonctionneront bien la plupart du temps. Cette optimisation est actuellement effectuée dans le compilateur natif par la série d'optimisations [flambda](https://v2.ocaml.org/manual/flambda.html), qui sera plus tard remplacé par [flambda2](https://github.com/ocaml-flambda/flambda-backend/tree/main/middle_end/flambda2) actuellement en développement. Découvrir et travailler sur un compilateur complexe comme celui d'OCaml n'a pas été jugé envisageable par mes tuteurs de stage, c'est la raison pour laquelle j'évolue sur un langage "jouet" qui n'est rien d'autre qu'une infime partie d'OCaml. La première moitié du stage a donc consisté à créer les outils (parseur, analyseur, interpréteur, etc.) nécessaires pour tester lors de la seconde partie du stage toutes les heuristiques d'inlining possibles qui me semblent pertinentes.

## Langage source

Dans le cadre du stage je ne traite que les fonctionnalités d'OCaml qui semblent nécessaires pour exploiter toutes les possibilités de l'inlining et suffisantes pour obtenir un langage Turing-complet. En particulier je me concentre uniquement sur le noyau fonctionnel d'OCaml, tout ce qui est impératif, orienté objet ou autre est ignoré. Ces choix pourront bien évidemment évoluer en fonction des besoins.

### Lambda-calcul

Les trois règles du lambda-calcul que sont les variables, l'application et l'abstraction sont évidemment des fonctionnalités indispensables.

### Opérations élémentaires

La manipulation des entiers et le branchement conditionnel sont incontournables pour réaliser des programmes dignes de ce nom.

### Fermetures (mutuellement) récursives

La prise en compte de la récursivité est fondamentale, premièrement en terme d'expressivité du langage (je vais être amené à réalisé des tests poussés), deuxièmement il est intéressant de prendre en compte la complexité liée à l'impossibilité d'inliner tous les appels récursifs potentiels. La récursivité mutuelle permet d'ajouter une couche de complexité notamment lors de l'analyse.

### Types Somme et pattern matching

La possibilité de construire des types Somme est une des fonctionnalités essentielles d'OCaml et permet de représenter quasiment n'importe quelle structure de données. De plus, de la même manière que le branchement conditionnel, le pattern matching se prête particulièrement bien à l'inlining puisque connaître le pattern peut permettre de simplifier de nombreuses branches et donc d'alléger un potentiel inlining.

## Langage utilisé

Il va de soit que OCaml est le meilleur langage pour créer un compilateur pour OCaml, la question ne se pose même pas, grâce notamment aux nombreux outils à disposition comme Menhir.

# Phases de compilation

Du premier jour de stage jusqu'à aujourd'hui j'ai considérablement augmenté le nombre de langages intermédiaires au fur et à mesure où la complexité des tâches à réaliser augmentait. J'ai fait des choix qui n'étaient pas toujours judicieux et dû faire machine arrière plusieurs fois ce qui m'a amené à me fixer les règles suivantes :

- Toujours, dans la mesure du raisonnable, pouvoir re-concrétiser après abstraction en gardant sous la main les informations nécessaires à la reconstruction (conserver par exemple la liste des alpha-conversions pour le débogage) ;
- Ne pas réaliser plusieurs abstractions à la fois, je pense qu'il est préférable d'avoir un langage intermédiaire en trop plutôt que d'avoir une transformation de l'un vers l'autre trop compliquée ;
- Éviter au mieux les constructions du langage qui ne sont pas utilisées lors d'une transformation mais utilisées dans la suivante, cas dans lequel il peut être pertinent de créer un langage intermédiaire.

Évidemment il est possible que ces règles que je m'applique de respecter ne soient valables que dans le cadre d'un projet de petite taille.

## Analyse lexicale

Les jetons de l'analyse lexicale sont générés comme toujours par OCamllex.

## Analyse syntaxique

L'AST est généré par Menhir.

## Analyse sémantique

Le CST est construit à partir de l'AST en résolvant les noms et les types (pour l'instant limités aux constructeurs).

### Alpha-conversion

Toutes les variables sont alpha-converties et retournées à part (la liste des substitutions ne fait pas partie du CST). Les variables libres du programme sont autorisées, également alpha-converties et retournée à part. L'acceptation de variables libres dans le programme permet à mes yeux de faciliter la gestion du non-déterminisme et d'éviter toute ambiguïté lors de l'analyse. En effet les entrées-sorties peuvent être vues comme des variables libres qui ne sont connues qu'au moment de l'exécution du programme, ce qui permet de s'assurer par exemple qu'un affichage sur la sortie ne serait pas optimisé (de la même manière je me pose la question à savoir si la mémoire peut être modélisée comme une variable libre, ce qui expliciterait le non-déterminisme des effets de bord).

### Indexation

Les noms des constructeurs ont reçus un index relatif à leur position dans la déclaration du type et les noms des constructeurs dans les pattern-matchings ont été remplacés par cet index.

### Typage

Pour l'instant je ne procède à aucune vérification de typage, c'est peu probable que cela change.

## CFG

Le CFG, qui n'est pas à proprement parler un CFG, transforme le CST en un ensemble (non ordonné) de basic blocs. L'idée est de perdre le moins d'informations possible du programme source tout en ayant sous la main un langage intermédiaire qui permette une analyse simple et puissante.

### Instruction

Une instruction est soit une déclaration soit un branchement. Une déclaration construit une valeur et l'associe à un identifiant unique. Les valeurs ne peuvent être construites qu'à partir de constantes ou identifiants. Un branchement représente le transfert d'un basic block à un autre que ce soit par le biais d'un appel (fermeteure), d'un retour de fonction (return) ou d'un saut conditionnel (if et match).

### Basic blocks

Chaque bloc est construit différemment en fonction de l'expression à partir de laquelle il est construit (fermeture, retour de fonction, branchement, etc.), est clos (il explicite les arguments dont il a besoin) et contient une suite de déclarations de variables suivies d'une instruction de branchement.

## CFG analysé

L'analyse est l'étape la plus compliquée et probablement la plus importante. Elle s'effectue au niveau du CFG afin d'exploiter la sémantique du langage (et donc de conserver certaines relations) et les informations sur les blocs. Sur conseil de mon tuteur, je réalise une analyse par zone d'allocation. Comme après alpha-conversion chaque nom de variable est unique, on peut identifier les valeurs par un ensemble de nom de variable (ce qui correspond aux endroits potentiels où elles ont été déclarées et initialisées). 

Pour exploiter l'analyse il m'a été nécessaire dans l'immédiat de cloner le CFG pour intégrer les informations issues de l'analyse. Cela concerne exclusivement les appels indirects transformés en appels directs mais pour lesquels je ne peux pas les transformer en saut (directs) pour rester conforme à la sémantique. La question de la pertinence d'un tel clone se pose, est-il préférable d'intégrer les appels directs dans le CFG (qui seront donc jamais utilisés dans la transformation CST vers CFG) ou de déterminer si un saut est direct en gardant sous la main les résultats de l'analyse lors de l'inlining (inlining ayant lieu dans la phase suivante) ?

### Abstractions

Actuellement j'utilise deux abstractions pour représenter toutes les valeurs du langage. La première pour les entiers qui est simplement le domaine singleton (deux entiers différents donnent Z). La deuxième pour les fermetures et les constructeurs est un environnement d'identifiants vers contextes, où l'identifiant correspond au pointeur de fonction dans le cas d'une fermeture ou au tag dans le cas d'un constructeur, et le contexte correspond respectivement aux variables libres ou aux arguments. Étant donné que les pointeurs de fonctions/tags ainsi que les contextes (ensemble de zones d'allocations) sont bornés, l'union des deux est garantie de converger.

### Preuve de terminaison

Les noms de variables, par extension les zones d'allocations, étant en nombre fini dans le programme, l'ensemble identifiant les valeurs est également fini. De la même manière, un contexte d'appel (ensemble fini de valeurs correspondant aux arguments et pointeur de bloc), est un ensemble fini étant donné que le nombre de blocs dans le programme est également borné. Reste la question épineuse de comment garantir que la pile d'appels ne croît pas infiniment. Afin de tenter d'obtenir une précision maximale, je détecte d'éventuels motifs sur la pile en regardant si 1..N contextes d'appels se répètent et le cas échéant je supprime la répétition. Par exemple la pile d'appels A::B::A::B::C::[] sera remplacée par A::B::C::[]. Après avoir implémenté cette méthode, mes tuteurs m'ont rapidement fait comprendre qu'elle ne pouvait pas garantir la terminaison. Pour la suite du stage je vais certainement devoir durcir la détection de motifs en passant à 0-CFA ou 1-CFA.

## ASM

L'ASM est construit à partir du CFG en faisant abstraction de quasiment tous les traits de langage propres à OCaml. A chaque construction de valeur du langage OCaml est associée une structure de données, la plupart d'entre elles devenant des n-uplets. Tous les types de blocs fusionnent en un seul en fixant la sémantique des sauts (passage de l'environnement comme argument) et chaque type de branchement est transformé en un saut (direct ou indirect) avec la possibilité d'ajouter des contextes d'appel sur la pile (seule l'instruction d'appel ajoute un contexte lors de cette transformation).

### Inlining

L'inlining a lieu sur le langage ASM car les modifications apportées peuvent casser la sémantique d'appel ce qui doit être représenter au niveau de la pile. Inliner un bloc consiste à intégrer son contenu dans le bloc appelant à la place de la dernière instruction (branchement). Chaque argument du bloc inliné est remplacé par la variable qui lui a été assignée lors du branchement par le bloc appelant. Pour l'instant (définitivement ?) seuls les appels directs peuvent être inlinés. Si lors de l'appel des contextes étaient empilés sur la pile, alors le branchement du bloc inliné en tiendra compte. En particulier, si le branchement du bloc inliné est un retour de fonction celui-ci dépilera la pile et deviendra un saut direct. Dans les autres cas les contextes du bloc appelant sont empilés sur les contextes du bloc appelé, ce qui permet d'avoir des sauts vers l'intérieur d'une fonction. 

# Heuristiques d'inlining

Les structures de données étant prêtes et à mes yeux suffisamment expressives pour permettre toute forme d'inlining, les heuristiques d'inlining en tant que telles seront traitées lors de la seconde moitié du stage. De nombreux tests seront à réaliser pour vérifier leur pertinence.

### Inlining partiel

Une heuristique possible sur laquelle je vais me concentrer sera l'[inlining partiel](https://developers.redhat.com/blog/2014/10/29/rhel7-gcc-optimizations-partial-inlining), c'est à dire potentiellement inliner seulement les premiers blocs d'une fonction. La manière dont j'ai représenté mon programme me permet de sauter à l'intérieur d'une fonction, ce qui n'est actuellement pas possible dans flambda2. La question est donc de savoir s'il existe des cas où cette heuristique pourrait être intéressante, que ce soit à la fois en terme d'optimisations de la taille et du temps d’exécution. Une première idée de situations intéressantes qui me vient à l'esprit est le (très) grand nombre de fonctions en OCaml qui matchent d'entrée un de leurs arguments. Le match en lui même n'est pas une opération spécialement coûteuse en terme d'espace, on transformerait ici un appel de fonction en un switch vers des blocs de celle-ci. De plus l'inliner peut permettre de gagner en informations sur le pattern, ce qui peut rendre possible de transformer le match en appel direct, les gains seraient considérables.

Un exemple qui me pousse à croire qu'une telle heuristique peut être prometteuse est fourni sur le site de [flambda](https://v2.ocaml.org/manual/flambda.html#ss:flambda-inlining-overview) :

```ocaml
let f b x =
  if b then x else ... big expression ...

let g x = f true x
```

En appliquant les transformations pour faire apparaître les continuations, les branchements et les manipulations de la pile on obtient ce pseudo-code qui se rapproche d'un langage assembleur (sans distinguer les appels directs des appels indirects) :

```ocaml
let rec f b x stack =
  if b then b0 x stack else b1 stack
and b0 x (k::stack) =
  k x stack
and b1 (k::stack) =
  k (... big expression ...) stack
and g x stack =
  f true x (r0::stack)
and r0 x1 (k::stack) =
  k x1 stack
```

Dans le cas présent la fonction `f` peut être vue comme un bloc d'une seule instruction. L'idée est donc d'inliner systématiquement ce bloc (pas la fonction complète, d'où la notion d'inlining partiel), et on obtient le code suivant où l'appel vers `f` est inliné dans `g` :

```ocaml
let rec f b x stack =
  if b then b0 x stack else b1 stack
and b0 x (k::stack) =
  k x stack
and b1 (k::stack) =
  k (... big expression ...) stack
and g x stack =
  if true then b0 x (r0::stack) else b1 (r0::stack)
and r0 x1 (k::stack) =
  k x1 stack
```

Cet inlining n'est possible que si toutes les branches du `if` empilent les mêmes contextes sur la pile, ce qui est normalement toujours le cas quand le code est bien construit. Avec cet inlining, indépendamment de la valeur de `b`, on économise un saut et on se donne davantage d'informations sur le contexte dans le bloc inliné qui pourraient être utiles à d'éventuelles simplifications. Évidemment au stade actuel cette heuristique ne reste qu'une hypothèse et il faudra vérifier à la fois sa faisabilité et son efficacité.
