# Pagerank

Projet réalisé en 2024 en première année à l'ENSEEIHT en langage Ada dans le cadre d'une évaluation orale.
Ce dernier consiste à implémenter un algorithme attribuant un rang à chaque page, représenté par un graphe orienté où les sommets correspondent aux pages et les arcs aux référencements.
Ainsi, à partir d'un vecteur dont les composantes représentent le rang des pages, on itère une opération de produit entre le vecteur de rangs et la matrice d'adjacences du graphe.
L'intérêt de ce sujet est de considérer que pour les grands graphes, une implémentation de matrices creuses peut considérablement réduire le temps de calcul.
