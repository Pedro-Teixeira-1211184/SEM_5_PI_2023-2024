import * as THREE from "three";
import {GLTFLoader} from "three/examples/jsm/loaders/GLTFLoader";
import Default_data from "./default_data";

/*
 * parameters = {
 *  url: String,
 *  credits: String,
 *  scale: Vector3,
 *  walkingSpeed: Float,
 *  initialDirection: Float,
 *  turningSpeed: Float,
 *  runningFactor: Float,
 *  keyCodes: { fixedView: String, firstPersonView: String, thirdPersonView: String, topView: String, viewMode: String, userInterface: String, miniMap: String, help: String, statistics: String, run: String, left: String, right: String, backward: String, forward: String, jump: String, yes: String, no: String, wave: String, punch: String, thumbsUp: String }
 * }
 */

export default class Robot {
    url: string;
    credits: string;
    scale: THREE.Vector3;
    walkingSpeed: number;
    initialDirection: number;
    turningSpeed: number;
    runningFactor: number;
    keyCodes: {
        fixedView: string,
        firstPersonView: string,
        thirdPersonView: string,
        topView: string,
        viewMode: string,
        userInterface: string,
        miniMap: string,
        help: string,
        statistics: string,
        run: string,
        left: string,
        right: string,
        backward: string,
        forward: string,
        jump: string,
        yes: string,
        no: string,
        wave: string,
        punch: string,
        thumbsUp: string
    };
    keyStates: {
        run: boolean,
        left: boolean,
        right: boolean,
        backward: boolean,
        forward: boolean,
        jump: boolean,
        yes: boolean,
        no: boolean,
        wave: boolean,
        punch: boolean,
        thumbsUp: boolean
    } = {
        run: false,
        left: false,
        right: false,
        backward: false,
        forward: false,
        jump: false,
        yes: false,
        no: false,
        wave: false,
        punch: false,
        thumbsUp: false
    }
    loaded: boolean = false;
    object!: THREE.Group;
    animations!: THREE.AnimationClip[];
    radius!: number;
    eyeHeight: number = Default_data.eyeHeight;
    scene!: THREE.Scene;

    constructor(scene: THREE.Scene, url: string, credits: string, initial_position: number[],scale: THREE.Vector3, walkingSpeed: number, initialDirection: number, turningSpeed: number, runningFactor: number, keyCodes: {
        fixedView: string,
        firstPersonView: string,
        thirdPersonView: string,
        topView: string,
        viewMode: string,
        userInterface: string,
        miniMap: string,
        help: string,
        statistics: string,
        run: string,
        left: string,
        right: string,
        backward: string,
        forward: string,
        jump: string,
        yes: string,
        no: string,
        wave: string,
        punch: string,
        thumbsUp: string
    }) {

        this.url = url;
        this.credits = credits;
        this.scale = scale;
        this.walkingSpeed = walkingSpeed;
        this.initialDirection = initialDirection;
        this.turningSpeed = turningSpeed;
        this.runningFactor = runningFactor;
        this.keyCodes = keyCodes;
        this.scene = scene;

        const loader = new GLTFLoader();

        loader.load(
            this.url,
            (gltf) => {
                // Loaded callback
                this.object = gltf.scene;
                this.object.scale.copy(this.scale);
                this.object.position.set(initial_position[0], 0.0, initial_position[1]);
                const box = new THREE.Box3();
                box.setFromObject(this.object); // This function may result in a larger box than strictly necessary: https://threejs.org/docs/#api/en/math/Box3.setFromObject

                // Compute the object size
                const size = new THREE.Vector3();
                box.getSize(size);

                // Adjust the object's oversized dimensions (hard-coded; see previous comments)
                size.x = 1.0;
                size.y = 4.4;
                size.z = 1.0;

                // Set the object's radius and eye height
                this.radius = size.x / 2.0 * this.scale.x;
                this.eyeHeight *= size.y * this.scale.y;
                this.animations = gltf.animations;
                this.scene.add(this.object);
                this.loaded = true;
            },
            (xhr) => {
                // Progress callback (optional)
                console.log((xhr.loaded / xhr.total) * 100 + '% loaded');
            },
            (error) => {
                // Error callback (optional)
                console.error('Error loading GLB model:', error);
            }
        );
    }
}
