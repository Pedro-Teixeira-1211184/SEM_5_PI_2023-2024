import * as THREE from "three";
import {GUI} from 'three/examples/jsm/libs/lil-gui.module.min.js'
import Lights from "./lights";
import Robot from "./robot";
import Animations from "./animations";

export default class User_interface {
    gui: GUI;

    constructor(scene: THREE.Scene, renderer: THREE.WebGLRenderer, lights: Lights, object: Robot, animations: Animations) {

        function shadowsCallback(enabled: boolean) {
            if (enabled) {
                renderer.shadowMap.enabled = true;
                renderer.shadowMap.type = THREE.PCFSoftShadowMap;
                renderer.shadowMap.needsUpdate = true;
            } else {
                renderer.shadowMap.enabled = false;
            }
        }

        /*
            function createEmoteCallback(animations: Animations, name: string) {
              const callback = function () {
                animations.fadeToAction(name, 0.2);
              };
              callbacks.push(callback);
              emotesFolder.add({play: callback}, "play").name(name);
            }

         */

        // Create the graphical user interface
        this.gui = new GUI();

        // Create the lights folder
        const lightsFolder = this.gui.addFolder("Lights");

        // Create the ambient light folder
        const ambientLightFolder = lightsFolder.addFolder("Ambient light");
        const ambientLight = lights.ambientLight;
        const ambientColor = {color: "#" + new THREE.Color(ambientLight.color).getHexString()};
        ambientLightFolder.add(lights.ambientLight, "intensity", 0.0, 1.0, 0.01);

        // Create point light #1 folder
        const pointLight1Folder = lightsFolder.addFolder("Point light #1");
        const pointLight1 = lights.pointLight1;
        pointLight1Folder.add(lights.pointLight1, "intensity", 0.0, 100.0, 1.0);
        pointLight1Folder.add(lights.pointLight1, "distance", 0.0, 20.0, 0.01);
        pointLight1Folder.add(lights.pointLight1.position, "x", -10.0, 10.0, 0.01);
        pointLight1Folder.add(lights.pointLight1.position, "y", 0.0, 20.0, 0.01);
        pointLight1Folder.add(lights.pointLight1.position, "z", -10.0, 10.0, 0.01);
        // Create point light #2 folder
        const pointLight2Folder = lightsFolder.addFolder("Point light #2");
        const pointLight2 = lights.pointLight2;
        pointLight2Folder.add(lights.pointLight2, "intensity", 0.0, 100.0, 1.0);
        pointLight2Folder.add(lights.pointLight2, "distance", 0.0, 20.0, 0.01);
        pointLight2Folder.add(lights.pointLight2.position, "x", -10.0, 10.0, 0.01);
        pointLight2Folder.add(lights.pointLight2.position, "y", 0.0, 20.0, 0.01);
        pointLight2Folder.add(lights.pointLight2.position, "z", -10.0, 10.0, 0.01);

        // Create the shadows folder
        /*
        const shadowsFolder = this.gui.addFolder("Shadows");
        shadowsFolder.add(renderer.shadowMap, "enabled").onChange(shadowsCallback);


        // Create the character folder
        const characterFolder = this.gui.addFolder("Character");

        // Create the emotes folder and add emotes
        const emotesFolder = characterFolder.addFolder("Emotes");
        const callbacks = [];
        for (let i = 0; i < animations.emotes.length; i++) {
          createEmoteCallback(animations, animations.emotes[i]);
        }

         */

    }

    setVisibility(visible: boolean) {
        if (visible) {
            this.gui.show();
        } else {
            this.gui.hide();
        }
    }
}
