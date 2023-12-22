import * as THREE from 'three';

export default class Door {

    constructor(textureUrl, color) {
        this.object = new THREE.Group();
        this.doorOpen = false; // Flag para controlar o estado da porta

        // Criar a parte principal da porta (um retângulo simples)
        const doorGeometry = new THREE.BoxGeometry(1, 1, 0.1);
        const loader = new THREE.TextureLoader();
        const texture = loader.load(textureUrl);
        const doorTexture = new THREE.MeshBasicMaterial({color: color, map: texture });
        const doorTextureMesh = new THREE.Mesh(doorGeometry, doorTexture);
        this.object.add(doorTextureMesh);

        // Adicionar maçaneta do lado direito (face da frente)
        const handleGeometry = new THREE.CylinderGeometry(0.05, 0.05, 0.2, 16);
        const handleMaterial = new THREE.MeshBasicMaterial({ color: 0x8b4513 }); // Marrom escuro

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

    // Função para alternar entre abrir e fechar a porta
    toggleDoor() {
        if (this.doorOpen) {
            this.closeDoor();
        } else {
            this.openDoor();
        }
    }

    // Função para abrir a porta
    openDoor() {
        if (!this.doorOpen) {
            const tween = new Tween({ rotationY: 0 })
                .to({ rotationY: -Math.PI / 2 }, 1000)
                .onUpdate(object => {
                    this.object.rotation.y = object.rotationY;
                })
                .start();

            this.doorOpen = true;
        }
    }

    // Função para fechar a porta
    closeDoor() {
        if (this.doorOpen) {
            const tween = new Tween({ rotationY: -Math.PI / 2 })
                .to({ rotationY: 0 }, 1000)
                .onUpdate(object => {
                    this.object.rotation.y = object.rotationY;
                })
                .start();

            this.doorOpen = false;
        }
    }
}