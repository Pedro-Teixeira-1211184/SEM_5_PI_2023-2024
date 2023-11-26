import IMapDTO from "../../../dto/IMapDTO";
import * as THREE from "three";

export default class Default_data {
  public static groundTextureUrl: string = 'assets/textures/ground.jpg';
  public static wallTextureUrl: string = 'assets/textures/wall.jpg';
  public static doorTextureUrl: string = 'assets/textures/door.jpg';
  public static bridgeTextureUrl: string = 'assets/textures/bridge.jpg';
  public static initialPosition = [3, 1]; // to change
  public static initialDirection = 0.0;
  public static robot_url: string = "assets/models/gltf/RobotExpressive/RobotExpressive.glb";
  public static credits = "Model and related code snippets created by <a href='https://www.patreon.com/quaternius' target='_blank' rel='noopener'>Tomás Laulhé</a>. CC0 1.0. Modified by <a href='https://donmccurdy.com/' target='_blank' rel='noopener'>Don McCurdy</a>.";
  public static eyeHeight: number = 0.8; // fraction of character height
  public static scale = new THREE.Vector3(0.2, 0.2, 0.2);
  public static walkingSpeed: number = 0.75;
  public static turningSpeed: number = 1.0; // Expressed in degrees / second
  public static runningFactor: number = 2.0; // Affects walking speed and turning speed
  public static ambientLight = {color: 0xffffff, intensity: 1.0};
  public static pointLight1 = {
    color: 0xffffff,
    intensity: 1.0,
    distance: 0.0,
    position: new THREE.Vector3(0.0, 0.0, 0.0)
  };
  public static pointLight2 = {
    color: 0xffffff,
    intensity: 1.0,
    distance: 0.0,
    position: new THREE.Vector3(0.0, 0.0, 0.0)
  };
  public static spotLight = {
    color: 0xffffff,
    intensity: 1.0,
    distance: 0.0,
    angle: Math.PI / 3.0,
    penumbra: 0.0,
    position: new THREE.Vector3(0.0, 0.0, 0.0),
    direction: 0.0
  };
  public static keyCodes = {
    fixedView: "Digit1",
    firstPersonView: "Digit2",
    thirdPersonView: "Digit3",
    topView: "Digit4",
    viewMode: "KeyV",
    userInterface: "KeyU",
    miniMap: "KeyM",
    help: "KeyH",
    statistics: "KeyS",
    run: "KeyR",
    left: "ArrowLeft",
    right: "ArrowRight",
    backward: "ArrowDown",
    forward: "ArrowUp",
    jump: "KeyJ",
    yes: "KeyY",
    no: "KeyN",
    wave: "KeyW",
    punch: "KeyP",
    thumbsUp: "KeyT"
  };
}
