import * as THREE from 'three';

export default class Door {

    constructor(textureUrl, color, name) {
        this.object = new THREE.Group();
        this.doorOpen = false; // Flag para controlar o estado da porta

        // Criar a parte principal da porta (um retângulo simples)
        const doorGeometry = new THREE.BoxGeometry(1, 1, 0.1);
        const loader = new THREE.TextureLoader();
        const texture = loader.load(textureUrl);
        const doorTexture = new THREE.MeshBasicMaterial({color: color, map: texture});

        const canvas = this.makeLabelCanvas( 100, 24, name);
        const labelTexture = new THREE.CanvasTexture( canvas );
        labelTexture.minFilter = THREE.LinearFilter;
        labelTexture.wrapS = THREE.ClampToEdgeWrapping;
        labelTexture.wrapT = THREE.ClampToEdgeWrapping;

        const labelMaterial = new THREE.SpriteMaterial( {
            map: labelTexture,
            transparent: true,
        } );

        this.doorMesh = new THREE.Mesh(doorGeometry, doorTexture);
        const label = new THREE.Sprite( labelMaterial );
        this.object.add(this.doorMesh);
        this.object.add(label);

        label.position.y = 0.9;
        label.position.z = 0.05;
        const labelBaseScale = 0.01;
        label.scale.x = canvas.width * labelBaseScale;
        label.scale.y = canvas.height * labelBaseScale;

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

    makeLabelCanvas( baseWidth, size, name ) {

        const borderSize = 2;
        const ctx = document.createElement( 'canvas' ).getContext( '2d' );
        const font = `${size}px bold Times New Roman`;
        ctx.font = font;
        // measure how long the name will be
        const textWidth = ctx.measureText( name ).width;

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
        ctx.fillRect( 0, 0, width, height );

        // scale to fit but don't stretch
        const scaleFactor = Math.min( 1, baseWidth / textWidth );
        ctx.translate( width / 2, height / 2 );
        ctx.scale( scaleFactor, 1 );
        ctx.fillStyle = 'white';
        ctx.fillText( name, 0, 0 );

        return ctx.canvas;

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
