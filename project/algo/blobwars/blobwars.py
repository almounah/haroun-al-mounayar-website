#!/usr/bin/env python3

"""Implemente le jeu de blob wars."""

from termcolor import colored
import os

def affiche_plateau(cases):
    """Affiche le plateau. Cases est un 64-uplet."""
    for i in range(8):
        for k in range(7):
            print(recupere_symbole(cases[k+8*i]), end="  ")
        print(recupere_symbole(cases[8*i+7]))


def met_a_jour_plateau(cases, chaine, saut, joueur):
    """Renvoie un nouveau plateau mis a jour avec coup."""
    L = convert_chaine(chaine)
    depart = L[0]
    arrive = L[1]
    if saut == 1:
        cases[depart] = str(depart)

    charactere = "j"+str(joueur)
    cases[arrive] = charactere
    autre_joueur = "j"+str(((joueur)+1)%2)
    for k in entourage(arrive) :
        if cases[k] == autre_joueur:
            cases[k] = charactere


def entourage(nombre):
    """renvoie une liste d'entourage du nombre."""
    if nombre == 0 :
        return [1, 8, 9]
    if nombre == 63:
        return [55 , 54, 62]
    if nombre == 56:
        return [57 , 49, 48]
    if nombre == 7:
        return [6 , 14,15]

    if nombre in [8 ,16, 24, 32, 40 ,48]:
        return [nombre+1, nombre+8, nombre-8, nombre+9, nombre-7]

    if nombre in [15, 23, 31, 39, 47, 55]:
        return [nombre-1, nombre+8, nombre-8, nombre-9, nombre+7]

    if nombre in [1, 2, 3, 4, 5, 6]:
        return [nombre-1, nombre+1, nombre+8, nombre+7 , nombre+9]

    if nombre in [57, 58, 59, 60, 61, 62]:
        return [nombre-1, nombre+1, nombre-8 , nombre-7, nombre-9]

    return [nombre-1, nombre+1, nombre-8, nombre+8, nombre-7, nombre+7, nombre+9, nombre-9]


def joue_possible(cases, joueur):
    """S'assure si le match n'est pas termine. Renvoie True si jeu possible."""
    charactere = 'j'+str(joueur)
    pion = []
    autre_joueur = "j"+str(((joueur)+1)%2)
    for k in range(len(cases)):
        if cases[k] == charactere:
            pion = pion + [k]

    if pion == []:
        return False

    for i in pion:
        compteur_entourage = 0
        for k in entourage(i):
            if cases[k] == autre_joueur:
                compteur_entourage = compteur_entourage + 1
        if compteur_entourage != len(entourage(i)):
            return True

    return False






def recupere_symbole(texte):
    """Retourne une chaine de charactaire unicode correspondant au symbole du joueur."""
    if texte == "j0":
        return colored(u"\u2B24 ", "red")
    if texte == "j1":
        return colored(u"\u2B24 ", "blue")
    elif int(texte)<10:
        res = "0{a}"
        return res.format(a = int(texte))
    else:
        return texte






def joue_coup(cases, joueur):
    """Demande a et renvoie ce que joue le joueur.
    """
    if joueur == 0:
        print("Joueur rouge veuiller entrer le numero de case")
    if joueur == 1:
        print("Joueur bleu veuiller entrer le numero de case")
    print("Le format doit etre 'depart arrive' ")
    chaine = input("C'est a vous: ")
    boollen, saut = coup_valide(cases, chaine)
    if boollen:
        return chaine, saut

    print("Format Invalide")
    return joue_coup(cases, joueur)


def coup_valide(cases, chaine):
    """S'assure si le coup joue est valide.

    En plus elle Renvoie saut = 1 si y a saut 0 sinon.
    """
    for k in chaine:
        if not (k in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", " "]):
            return False, 1
    L = convert_chaine(chaine)
    if len(L) != 2:
        return False, 1

    depart = L[0]
    arrive = L[1]
    charactere = "j"+str(joueur)
    if cases[depart] == charactere:
            if cases[arrive] == "j1" or cases[arrive] == "j0":
                return False, 1
            lignedep, colonnedep = depart//8, depart%8
            lignearr, colonnearr = arrive//8, arrive%8
            if abs(lignedep-lignearr) <= 2 and abs(colonnedep-colonnearr) <= 2:
                if abs(lignedep-lignearr) == 2 or abs(colonnedep-colonnearr) == 2:
                    return True, 1
                return True, 0
    return False, 1


def coord(nombre):
    """Renvoie les coord ligne colonne d'un nombre dans la case"""



def convert_chaine(chaine):
    """Retourne departe arrive de chaine dans une liste."""
    nombre = ""
    L=[]
    for i in chaine:
        a = i
        if a == " ":
            L=L+[int(nombre)]
            nombre=" "
            continue
        nombre = nombre+a
    L = L+[int(nombre)]
    return L


joueur = 0
cases = [str(k) for k in range(0, 64)]
cases[0] = 'j0'
cases[7] = 'j0'
cases[63] = 'j1'
cases[56] = 'j1'
affiche_plateau(cases)

while joue_possible(cases, joueur):
    chaine, saut = joue_coup(cases, joueur)
    os.system('clear')
    met_a_jour_plateau(cases, chaine, saut, joueur)
    joueur = (joueur + 1) % 2
    affiche_plateau(cases)

joueur = (joueur + 1) % 2
if joue_possible(cases, joueur):
    joueur0_nombre = 0
    joueur1_nombre = 0
    for k in range(len(cases)):
        if cases[k] == "j0":
            joueur0_nombre = joueur0_nombre + 1
        if cases[k] == "j1":
            joueur1_nombre = joueur1_nombre + 1
    if joueur1_nombre > joueur0_nombre:
        joueur = 1
    if joueur1_nombre < joueur0_nombre:
        joueur = 0
    if joueur0_nombre == joueur1_nombre:
        joueur = "0 et 1"


print("Le joueur", joueur, "gagne")
