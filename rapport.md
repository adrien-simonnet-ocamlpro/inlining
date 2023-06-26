# Rapport d'avancement

## Langage source

Le langage source est un sous-ensemble des fonctionnalités d'OCaml. Dans le cadre du stage je ne traite que celles qui sont intéressantes d'un point de vue de l'inlining. En particulier le noyeau impératif d'OCaml est ignoré.

### Opérations élémentaires

La manipulation des entiers, la déclaration de variables ou de fonctions (unaires) et le branchement conditionnel sont évidemment des fonctionnalités indispensables.

### Fermetures (mutuellement) récursives

La prise en compte de la récursivité est fondamentale, du fait de l'impossibité d'inliner tous les appels potentiels. Prendre en compte la récursivité mutuelle permet d'ajouter une couche de complexité notamment lors de l'analyse.

### Types Somme et pattern matching

Le pattern matching sur des types Somme est une des fonctionnalités de base d'OCaml. De plus, de la même manière que les autres branchements conditionnels, elle se prête particulièrement bien à l'inlining puisque connaître le pattern permet probablement de simplifier de nombreuses branches et donc d'alléger un potentiel inlining.

## Langages intermédiaires

### AST

### CST

Le CST est un langage intermédiaire à expressions presque identique à l'AST à l'exception que toutes les variables ont été alpha-converties, les noms des constructeurs ont reçus un identifiant unique et les noms des constructeurs des pattern-matchings ont été remplacés par cet identifiant.

### CPS

Le CPS est un langage intermédiaire à instructions qui est une version "impérative" du CST. Chaque

### ASM

La structure du langage ASM est très semblable à celle de CPS, à l'exception d'un niveau d'abstraction beaucoup plus important. Les structures propres à OCaml disparaissent et sont généralisées. Tous les blocs (fermetures, conditionnels, pattern-matching, retour de fonction) fusionnent en un seul. C'est ici que la sémantique des appels est fixées (passage de l'environnement comme argument). Pour conserver de manière idéale la sémantique des appels, chaque type de branchement a la possibilité d'ajouter des contextes d'appel sur la pile.

## Analyse

L'analyse est l'étape la plus compliquée

## Propagation



## Inlining

Inliner un bloc consiste à intégrer son contenu dans le bloc appelant à la place de la dernière instruction de branchement. Chaque argument du bloc inliné est remplacé par la variable qui lui a été assignée lors du branchement par le bloc appelant. Pour l'instant, et en toute logique probablement définitivement, seuls les appels directs peuvent être inlinés. Si lors de l'appel des contextes étaient empilés sur la pile, alors le branchement du bloc inliné en tiendra compte. En particulier, si le branchement du bloc inliné est un return celui-ci dépilera la pile et deviendra un appel direct. Dans les autres cas les contextes du bloc appelant sont empilés sur les contextes du bloc appelé, ce qui permet d'avoir des sauts vers l'intérieur d'une fonction. 

## Heuristiques

Les structures de données étant prêtes et à mes yeux suffisement expressives pour permettre toute forme d'inlining, les heuristiques d'inlining en tant que telles seront traitées lors de la seconde moitié du stage. De nombreux tests seront à réaliser pour vérifier leur pertinence.

### Inlining partiel

Une heuristique possible sur laquelle je vais me concentrer sera l'inlining partiel, c'est à dire potentiellement inliner seulement les premiers blocs d'une fonction. La manière dont j'ai représenté mon programme me permet de sauter à l'intérieur d'une fonction, ce qui n'est actuellement pas possible dans flambda2. La question est donc de savoir s'il existe des cas où cette heuristique pourrait être intéressante, que ce soit à la fois en terme d'optimisations de la taille et du temps d'éxécution. Une première idée de situations intéressantes qui me vient à l'esprit est le (très) grand nombre de fonctions en OCaml qui matchent d'entrée un de leurs arguments. Le match en lui même n'est pas une opération spécialement coûteuse en terme d'espace, on transfomerait ici un appel de fonction en un switch vers des blocs de celle-ci. De plus l'inliner peut permettre de gagner en informations sur le pattern, ce qui peut rendre possible de transformer le match en appel direct, les gains seraient considérables.

### Outlining

