import * as THREE from 'three';

export default class Elevator {

    constructor() {
        this.object = new THREE.Group();
        this.isOpen = false;

        // Criar a caixa principal do elevador
        const elevatorGeometry = new THREE.BoxGeometry(1, 1, 1);
        const elevatorMaterial = new THREE.MeshBasicMaterial({ color: 0x808080 });
        const elevatorMesh = new THREE.Mesh(elevatorGeometry, elevatorMaterial);
        this.object.add(elevatorMesh);

        // Criar as portas do elevador
        const doorGeometry = new THREE.BoxGeometry(0.5, 1, 0.01);
        const doorMaterial = new THREE.MeshBasicMaterial({ color: 0x443c68 });

        // Porta esquerda
        this.doorLeft = new THREE.Mesh(doorGeometry, doorMaterial);
        this.doorLeft.position.set(-0.25, 0, 0.505);
        this.object.add(this.doorLeft);

        // Porta direita
        this.doorRight = new THREE.Mesh(doorGeometry, doorMaterial);
        this.doorRight.position.set(0.25, 0, 0.505);

        this.object.add(this.doorRight);
    }

    toggleDoor() {
        this.isOpen = !this.isOpen;

        if (this.isOpen) {
            this.doorLeft.position.set(-0.75, 0, 0.505);
            this.doorRight.position.set(0.75, 0, 0.505);
        } else {
            this.doorLeft.position.set(-0.25, 0, 0.505);
            this.doorRight.position.set(0.25, 0, 0.505);
        }
    }
}
