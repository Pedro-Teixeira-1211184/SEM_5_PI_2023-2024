import * as THREE from 'three';

export default class Door {

    constructor(textureUrl, color) {
        this.object = new THREE.Group();
        this.doorOpen = false; // Flag para controlar o estado da porta

        // Criar a parte principal da porta (um retângulo simples)
        const doorGeometry = new THREE.BoxGeometry(1, 1, 0.1);
        const loader = new THREE.TextureLoader();
        const texture = loader.load(textureUrl);
        const doorTexture = new THREE.MeshBasicMaterial({color: color, map: texture});
        this.doorMesh = new THREE.Mesh(doorGeometry, doorTexture);
        this.object.add(this.doorMesh);

        // Adicionar maçaneta do lado direito (face da frente)
        const handleGeometry = new THREE.CylinderGeometry(0.05, 0.05, 0.2, 16);
        const handleMaterial = new THREE.MeshBasicMaterial({color: 0x8b4513}); // Marrom escuro

        // Maçaneta do lado direito (face da frente)
        const handleMeshFrontRight = new THREE.Mesh(handleGeometry, handleMaterial);
        handleMeshFrontRight.position.set(0.5, 0, 0.05); // Ajustar posição da maçaneta
        this.object.add(handleMeshFrontRight);

        // Maçaneta do lado direito (face de trás)
        const handleMeshBackRight = new THREE.Mesh(handleGeometry, handleMaterial);
        handleMeshBackRight.rotateY(Math.PI); // Gira a maçaneta para a face de trás
        handleMeshBackRight.position.set(0.5, 0, -0.05); // Ajustar posição da maçaneta na face de trás
        this.object.add(handleMeshBackRight);
    }

    openDoor() {
        if (!this.doorOpen) {
            // Adicione aqui a lógica de animação para abrir a porta
            this.doorMesh.position.x -= 0.5; // Compensa a rotação movendo a porta para a esquerda
            this.doorMesh.position.z += 0.5; // Compensa a rotação movendo a porta para frente
            this.doorMesh.rotation.y = Math.PI / 2; // Rotaciona a porta para abrir
            this.doorOpen = true;
        }
    }

    closeDoor() {
        if (this.doorOpen) {
            // Adicione aqui a lógica de animação para fechar a porta
            this.doorMesh.position.x += 0.5; // Compensa a rotação movendo a porta de volta para a direita
            this.doorMesh.position.z -= 0.5; // Compensa a rotação movendo a porta de volta para trás
            this.doorMesh.rotation.y = 0; // Rotaciona a porta de volta para fechar
            this.doorOpen = false;
        }
    }

    toggleDoor() {
        if (this.doorOpen) {
            this.closeDoor();
        } else {
            this.openDoor();
        }
    }
}
