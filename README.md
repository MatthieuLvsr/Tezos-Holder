
# Projet Tezos Holder

## À propos

Ce projet comprend deux contrats intelligents développés avec LIGO pour la blockchain Tezos :

1. **Token**: Un contrat FA2 standard qui implémente les fonctionnalités de base d'un token FA2, y compris les transferts de tokens et la gestion des opérateurs.
2. **Caller**: Un contrat qui interagit avec le contrat Token pour permettre aux utilisateurs de déposer des tokens dans un registre de contrat, et de les réclamer après une période de gel.

## Prérequis

Avant de déployer les contrats, assurez-vous d'avoir :

- Node.js installé sur votre machine.
- Un compte Tezos avec des fonds disponibles pour le déploiement (sur Ghostnet pour les tests).
- Créé un fichier `.env` à la racine de votre projet avec les variables `SECRET_KEY` et `ADMIN_ADDRESS` configurées.

## Déploiement

Les scripts de déploiement utilisent [Taquito](https://tezostaquito.io/), une bibliothèque légère pour développer des applications sur Tezos. Pour déployer les contrats `Token` et `Caller`, suivez ces étapes :

1. Installez les dépendances en exécutant `make install` ou `ligo install` dans le répertoire du projet puis `npm install` depuis le répertoire deploy.
2. Compilez vos contrats LIGO en Michelson et générez les fichiers JSON correspondants en utilisant la commande `make compile` ou `ligo compile` (assurez-vous que les fichiers compilés se trouvent dans le répertoire `compiled`).
3. Configurez le fichier `.env` à la racine du projet avec vos clés et adresses comme suit :
   ```
   SECRET_KEY=edsk...
   ADMIN_ADDRESS=tz1...
   ```
4. Exécutez le script de déploiement `make deploy` depuis la racine ou bien `npm run deploy` depuis le dosier deploy

## Contribuer

Les contributions à ce projet sont les bienvenues. Vous pouvez contribuer en signalant des problèmes, en suggérant des améliorations, ou en soumettant des Pull Requests.

## Licence

Ce projet est distribué sous la Licence MIT. Voir le fichier `LICENSE` pour plus d'informations.

## Contact

Pour toute question ou suggestion, n'hésitez pas à créer une issue sur ce dépôt GitHub.
