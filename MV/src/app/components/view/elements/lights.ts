import * as THREE from "three";

/*
 * parameters = {
 *  ambientLight: { color: Integer, intensity: Float },
 *  pointLight1: { color: Integer, intensity: Float, distance: Float, position: Vector3 },
 *  pointLight2: { color: Integer, intensity: Float, distance: Float, position: Vector3 },
 *  spotLight: { color: Integer, intensity: Float, distance: Float, angle: Float, penumbra: Float, position: Vector3, direction: Float }
 * }
 */

export default class Lights {
    object: THREE.Group;
    pointLight1: {
        color: number,
        intensity: number,
        distance: number,
        position: THREE.Vector3
    };
    pointLight2: {
        color: number,
        intensity: number,
        distance: number,
        position: THREE.Vector3
    };
    ambientLight: {
        color: number,
        intensity: number
    };
    spotLight: {
        color: number,
        intensity: number,
        distance: number,
        angle: number,
        penumbra: number,
        position: THREE.Vector3,
        direction: number
    };

    constructor(ambientLight: {
        color: number,
        intensity: number
    }, pointLight1: {
        color: number,
        intensity: number,
        distance: number,
        position: THREE.Vector3
    }, pointLight2: {
        color: number,
        intensity: number,
        distance: number,
        position: THREE.Vector3
    }, spotLight: {
        color: number,
        intensity: number,
        distance: number,
        angle: number,
        penumbra: number,
        position: THREE.Vector3,
        direction: number
    }) {

        this.ambientLight = ambientLight;
        this.pointLight1 = pointLight1;
        this.pointLight2 = pointLight2;
        this.spotLight = spotLight;

        // Create a group of objects
        this.object = new THREE.Group();

        // Create the ambient light
        const ambLight = new THREE.AmbientLight(this.ambientLight.color, this.ambientLight.intensity);

        this.object.add(ambLight);

        // Create the first point light and turn on shadows for this light
        const pointLight3 = new THREE.PointLight(this.pointLight1.color, this.pointLight1.intensity, this.pointLight1.distance);
        pointLight3.position.set(this.pointLight1.position.x, this.pointLight1.position.y, this.pointLight1.position.z);
        pointLight3.castShadow = true;

        // Set up shadow properties for this light
        pointLight3.shadow.mapSize.width = 512;
        pointLight3.shadow.mapSize.height = 512;
        pointLight3.shadow.camera.near = 5.0;
        pointLight3.shadow.camera.far = 15.0;
        this.object.add(pointLight3);

        // Create the second point light and turn on shadows for this light
        const pointLight4 = new THREE.PointLight(this.pointLight2.color, this.pointLight2.intensity, this.pointLight2.distance);
        pointLight4.position.set(this.pointLight2.position.x, this.pointLight2.position.y, this.pointLight2.position.z);
        pointLight4.castShadow = true;

        // Set up shadow properties for this light
        pointLight4.shadow.mapSize.width = 512;
        pointLight4.shadow.mapSize.height = 512;
        pointLight4.shadow.camera.near = 5.0;
        pointLight4.shadow.camera.far = 15.0;
        this.object.add(pointLight4);
    }
}
