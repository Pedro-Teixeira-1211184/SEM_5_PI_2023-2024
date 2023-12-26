import * as THREE from 'three';

export default class Elevator {

    constructor() {
        this.object = new THREE.Group();
        this.isOpen = false;

        // Criar a caixa principal do elevador
        const elevatorGeometry = new THREE.BoxGeometry(1, 1, 1);
        const elevatorMaterial = new THREE.MeshBasicMaterial({color: 0x808080});
        const elevatorMesh = new THREE.Mesh(elevatorGeometry, elevatorMaterial);
        this.object.add(elevatorMesh);

        // Criar as portas do elevador
        const doorGeometry = new THREE.BoxGeometry(0.5, 1, 0.01);
        const doorMaterial = new THREE.MeshBasicMaterial({color: 0x443c68});

        // Porta esquerda
        this.doorLeft = new THREE.Mesh(doorGeometry, doorMaterial);
        this.doorLeft.position.set(-0.25, 0, 0.505);
        this.object.add(this.doorLeft);

        // Porta direita
        this.doorRight = new THREE.Mesh(doorGeometry, doorMaterial);
        this.doorRight.position.set(0.25, 0, 0.505);
        this.object.add(this.doorRight);

        const canvas = this.makeLabelCanvas(100, 24, 'Elevator');
        const labelTexture = new THREE.CanvasTexture(canvas);
        labelTexture.minFilter = THREE.LinearFilter;
        labelTexture.wrapS = THREE.ClampToEdgeWrapping;
        labelTexture.wrapT = THREE.ClampToEdgeWrapping;

        const labelMaterial = new THREE.SpriteMaterial({
            map: labelTexture,
            transparent: true,
        });

        const label = new THREE.Sprite(labelMaterial);
        this.object.add(label);

        label.position.y = 0.9;
        label.position.z = 0.05;
        const labelBaseScale = 0.01;
        label.scale.x = canvas.width * labelBaseScale;
        label.scale.y = canvas.height * labelBaseScale;
    }

    makeLabelCanvas(baseWidth, size, name) {

        const borderSize = 2;
        const ctx = document.createElement('canvas').getContext('2d');
        const font = `${size}px bold Times New Roman`;
        ctx.font = font;
        // measure how long the name will be
        const textWidth = ctx.measureText(name).width;

        const doubleBorderSize = borderSize * 2;
        const width = baseWidth + doubleBorderSize;
        const height = size + doubleBorderSize;
        ctx.canvas.width = width;
        ctx.canvas.height = height;

        // need to set font again after resizing canvas
        ctx.font = font;
        ctx.textBaseline = 'middle';
        ctx.textAlign = 'center';

        ctx.fillStyle = 'blue';
        ctx.fillRect(0, 0, width, height);

        // scale to fit but don't stretch
        const scaleFactor = Math.min(1, baseWidth / textWidth);
        ctx.translate(width / 2, height / 2);
        ctx.scale(scaleFactor, 1);
        ctx.fillStyle = 'white';
        ctx.fillText(name, 0, 0);

        return ctx.canvas;

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
