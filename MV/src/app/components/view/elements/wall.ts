import * as THREE from "three";

/*
 * parameters = {
 *  textureUrl: String
 * }
 */

export default class Wall {
    textureUrl: string;
    object: THREE.Group;

    constructor(textureUrl: string) {
        this.textureUrl = textureUrl;

        // Create a texture
        const texture = new THREE.TextureLoader().load(this.textureUrl);
        texture.colorSpace = THREE.SRGBColorSpace;
        texture.magFilter = THREE.LinearFilter;
        texture.minFilter = THREE.LinearMipmapLinearFilter;

        // Create a wall (seven faces) that casts and receives shadows

        // Create a group of objects
        this.object = new THREE.Group();

        // Create the front face (a rectangle)
        let geometry = new THREE.PlaneGeometry(0.95, 1.0);
        let material = new THREE.MeshBasicMaterial({color: 0xffffff, map: texture});
        let face = new THREE.Mesh(geometry, material);
        face.position.set(0.0, 0.0, 0.025);
        face.castShadow = true;
        face.receiveShadow = true;
        this.object.add(face);

        // Create the rear face (a rectangle)
        let face2 = new THREE.Mesh().copy(face, false);
        face2.rotateY(Math.PI);
        face2.position.set(0.0, 0.0, -0.025);
        this.object.add(face2);

        // Create the two left faces (a four-triangle mesh)
        let points = new Float32Array([
            -0.475, -0.5, 0.025,
            -0.475, 0.5, 0.025,
            -0.5, 0.5, 0.0,
            -0.5, -0.5, 0.0,

            -0.5, 0.5, 0.0,
            -0.475, 0.5, -0.025,
            -0.475, -0.5, -0.025,
            -0.5, -0.5, 0.0
        ]);
        let normals = new Float32Array([
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,

            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707
        ]);
        let indices = [
            0, 1, 2,
            2, 3, 0,
            4, 5, 6,
            6, 7, 4
        ];
        let geometry2 = new THREE.BufferGeometry().setAttribute("position", new THREE.BufferAttribute(points, 3)); // itemSize = 3 because there are 3 values (X, Y and Z components) per vertex
        geometry2.setAttribute("normal", new THREE.BufferAttribute(normals, 3));
        geometry2.setIndex(indices);
        material = new THREE.MeshBasicMaterial({color: 0x6b554b});
        let face3 = new THREE.Mesh(geometry2, material);
        face3.castShadow = true;
        face3.receiveShadow = true;
        this.object.add(face3);

        // Create the two right faces (a four-triangle mesh)
        let face4 = new THREE.Mesh().copy(face, false);
        face4.rotateY(Math.PI);
        this.object.add(face4);

        // Create the top face (a four-triangle mesh)
        points = new Float32Array([
            -0.5, 0.5, 0.0,
            -0.475, 0.5, 0.025,
            -0.475, 0.5, -0.025,
            0.475, 0.5, 0.025,
            0.475, 0.5, -0.025,
            0.5, 0.5, 0.0
        ]);
        normals = new Float32Array([
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
        ]);
        indices = [
            0, 1, 2,
            2, 1, 3,
            3, 4, 2,
            4, 3, 5
        ];
        let geometry3 = new THREE.BufferGeometry().setAttribute("position", new THREE.BufferAttribute(points, 3)); // itemSize = 3 because there are 3 values (X, Y and Z components) per vertex
        geometry3.setAttribute("normal", new THREE.BufferAttribute(normals, 3));
        geometry3.setIndex(indices);
        let face5 = new THREE.Mesh(geometry3, material);
        face5.castShadow = true;
        face5.receiveShadow = true;
        this.object.add(face5);
    }
}
