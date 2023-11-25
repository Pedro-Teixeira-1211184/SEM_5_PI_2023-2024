import * as THREE from "three";

export default class Animations {
  states: string[];
  emotes: string[];
  mixer: THREE.AnimationMixer;
  actions: any;
  activeName: string;
  actionInProgress: boolean;

  constructor(object: THREE.Object3D, animations: THREE.AnimationClip[]) {
    this.states = ["Idle", "Walking", "Running", "Dance", "Death", "Sitting", "Standing"];
    this.emotes = ["Jump", "Yes", "No", "Wave", "Punch", "ThumbsUp"];

    this.mixer = new THREE.AnimationMixer(object);
    this.actionInProgress = false;

    this.actions = {};
    for (let i = 0; i < animations.length; i++) {
      const clip = animations[i];
      const action = this.mixer.clipAction(clip);
      this.actions[clip.name] = action;
      if (this.states.indexOf(clip.name) >= 4 || this.emotes.indexOf(clip.name) >= 0) {
        action.clampWhenFinished = true;
        action.loop = THREE.LoopOnce;
      }
    }
    this.activeName = "Idle";
    this.actions[this.activeName].play();
  }

  fadeToAction(name: string, duration: number) {
    if (this.activeName != name && !this.actionInProgress) {
      const previousName = this.activeName;
      this.activeName = name;
      this.actions[previousName].fadeOut(duration);
      this.actions[this.activeName]
        .reset()
        .setEffectiveTimeScale(1)
        .setEffectiveWeight(1)
        .fadeIn(duration)
        .play();
      // Some actions must not be interrupted
      if (this.activeName != "Idle" && this.activeName != "Walking" && this.activeName != "Running") {
        this.mixer.addEventListener("finished", event => this.actionFinished());
        this.actionInProgress = true;
      }
    }
  }

  actionFinished() {
    if (this.actionInProgress) {
      this.actionInProgress = false;
      this.mixer.removeEventListener("finished", event => this.actionFinished());
    }
  }

  update(deltaT: number) {
    if (this.mixer) {
      this.mixer.update(deltaT);
    }
  }
}
