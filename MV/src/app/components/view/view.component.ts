import {AfterViewInit, Component, ElementRef, inject, Input, OnInit, ViewChild} from '@angular/core';
import * as THREE from "three";
import Orientation from "./elements/orientation";
import Camera from "./elements/camera";
import Floor from "./elements/floor";
import Default_data from "./elements/default_data";
import IMapDTO from "../../dto/IMapDTO";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {MapService} from "../../services/map/map.service";
import Robot from "./elements/robot";
import Lights from "./elements/lights";
import Animations from "./elements/animations";
import {Door} from "./elements/door";

@Component({
  selector: 'app-view',
  templateUrl: './view.component.html',
  styleUrls: ['./view.component.scss']
})
export class ViewComponent implements OnInit {

  title = 'View';
  maps: IMapDTO[] = [];
  buildings: string[] = [];
  floors: number[] = [];
  option!: IMapDTO;

  service = inject(MapService);

  form!: FormGroup;

  isFormVisible: boolean = true;
  showingTable: boolean = false;
  isRunning: boolean = false;

  ngOnInit(): void {
    this.form = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorCode: new FormControl('', [Validators.required]),
      robotX: new FormControl('', [Validators.required]),
      robotZ: new FormControl('', [Validators.required]),
    });
  }

  constructor() {
    this.getMaps();
  }

  private async getMaps() {
    this.maps = await this.service.getMaps();
    // different buildings
    for (let map of this.maps) {
      if (!this.buildings.includes(map.buildingCode)) {
        this.buildings.push(map.buildingCode);
      }
    }
  }

  getFloorsOfBuilding() {
    this.floors = [];
    for (let map of this.maps) {
      if (map.buildingCode == this.buildingCode?.value) {
        this.floors.push(map.floorNumber);
      }
    }
    //order floors ascending
    this.floors.sort((a, b) => a - b);
  }

  get buildingCode() {
    return this.form.get('buildingCode');
  }

  get floorCode() {
    return this.form.get('floorCode');
  }

  get robotX() {
    return this.form.get('robotX');
  }

  get robotZ() {
    return this.form.get('robotZ');
  }

  submitCounter: number = 0;

  submit() {
    if (this.submitCounter > 0) {
      if (this.form.valid) {
        if (this.buildingCode?.value != null && this.floorCode?.value != null) {
          for (let map of this.maps) {
            if (map.buildingCode == this.buildingCode?.value && map.floorNumber == this.floorCode?.value) {
              this.option = map;
            }
          }
          this.scene.remove(this.floor.object);
          this.floor = new Floor(Default_data.groundTextureUrl, new THREE.Vector3(-3.5, 10, 2.5), this.option);
          this.scene.add(this.floor.object);
          const pos: number[] = [this.robotX?.value, this.robotZ?.value]
          this.robot.object.position.set(pos[0], 0, pos[1]);
          this.robot.object.rotation.y = 0;
          this.isFormVisible = false;
          this.showingTable = true;
          this.render();
        }
      }
    } else {
      if (this.form.valid) {
        // se as todos os campos do formulário estiverem preenchidos
        if (this.buildingCode?.value != null && this.floorCode?.value != null && this.robotX?.value != null && this.robotZ?.value != null) {
          for (let map of this.maps) {
            if (map.buildingCode == this.buildingCode?.value && map.floorNumber == this.floorCode?.value) {
              this.option = map;
              this.isFormVisible = false;
              this.showingTable = true;
              this.createScene();
              this.render();
              this.submitCounter++;
              break;
            }
          }
        }
      }
    }
  }

  @ViewChild('myCanvas') private canvasRef!: ElementRef;

  //? Helper Properties (Private properties);
  private get canvas(): HTMLCanvasElement {
    return this.canvasRef.nativeElement;
  }


  private floor!: Floor;
  private robot!: Robot;
  private light!: Lights;
  private renderer!: THREE.WebGLRenderer;
  private scene: THREE.Scene = new THREE.Scene();
  camera!: Camera;
  private cameraUsed: any = null;
  selectedProjection: string = 'perspective';

  /**
   * Create the scene
   *
   * @private
   * @memberof CubeComponent
   */
  private createScene(): void {
    //* Scene
    // Create the scene
    this.scene = new THREE.Scene();

    this.floor = new Floor(Default_data.groundTextureUrl, new THREE.Vector3(-3.5, 10, 2.5), this.option);
    this.scene.add(this.floor.object);
    const pos: number[] = [this.robotX?.value, this.robotZ?.value]
    this.robot = new Robot(this.scene, Default_data.robot_url, Default_data.credits, pos, Default_data.scale, Default_data.walkingSpeed, Default_data.initialDirection, Default_data.turningSpeed, Default_data.runningFactor, Default_data.keyCodes);
    this.light = new Lights(Default_data.ambientLight, Default_data.pointLight1, Default_data.pointLight2, Default_data.spotLight);
    this.scene.add(this.light.object);

    // key change
    // Register the event handler to be called on key down
    document.addEventListener("keydown", event => this.keyChange(event, true));

    // Register the event handler to be called on key release
    document.addEventListener("keyup", event => this.keyChange(event, false));

    //*Camera
    this.camera = new Camera(window.innerWidth, window.innerHeight, "fixed", new THREE.Vector4(0.0, 0.0, 1.0, 1.0), new THREE.Vector3(0.0, 0.0, 0.0),
      new Orientation(135.0, -45.0), new Orientation(-180.0, -90.0), new Orientation(180.0, 0.0), 8.0, 4.0, 16.0, 0.5, 0.5, 2.0,
      45.0, 0.01, 100.0);
    this.cameraUsed = this.camera.object;
    //* Renderer
    // Use canvas element in template
    this.renderer = new THREE.WebGLRenderer({canvas: this.canvas});
    this.renderer.setPixelRatio(devicePixelRatio);
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    this.renderer.domElement.addEventListener("wheel", event => this.mouseWheel(event));
    // Register the event handler to be called on mouse down
    this.renderer.domElement.addEventListener("mousedown", event => this.mouseDown(event));
    // Register the event handler to be called on mouse move
    this.renderer.domElement.addEventListener("mousemove", event => this.mouseMove(event));
    // Register the event handler to be called on mouse up
    this.renderer.domElement.addEventListener("mouseup", event => this.mouseUp(event));
  }


  /**
   * Render the scene
   */
  private render() {
    requestAnimationFrame(() => this.render());
    this.update();
  }

  changeCamera() {
    if (this.selectedProjection === 'perspective') {
      this.cameraUsed = this.camera.object;
    } else if (this.selectedProjection === 'orthographic') {
      this.cameraUsed = this.camera.orthographic;
    }
  }

  changeFloor() {
    this.isFormVisible = true;
    this.showingTable = false;
  }

  private keyChange(event: KeyboardEvent, state: boolean) {
    // Allow digit and arrow keys to be used when entering numbers
    /*
    if (["horizontal", "vertical", "distance", "zoom"].indexOf(event.target.id) < 0) {
      event.target.blur();
    }

     */
    if (document.activeElement == document.body) {
      // Prevent the "Space" and "Arrow" keys from scrolling the document's content
      if (event.code == "Space" || event.code == "ArrowLeft" || event.code == "ArrowRight" || event.code == "ArrowDown" || event.code == "ArrowUp") {
        event.preventDefault();
      }
      /*
      if (event.code == this.player.keyCodes.fixedView && state) { // Select fixed view
        this.setActiveViewCamera(this.fixedViewCamera);
      } else if (event.code == this.player.keyCodes.firstPersonView && state) { // Select first-person view
        this.setActiveViewCamera(this.firstPersonViewCamera);
      } else if (event.code == this.player.keyCodes.thirdPersonView && state) { // Select third-person view
        this.setActiveViewCamera(this.thirdPersonViewCamera);
      } else if (event.code == this.player.keyCodes.topView && state) { // Select top view
        this.setActiveViewCamera(this.topViewCamera);
      }

      if (event.code == this.robot.keyCodes.viewMode && state) { // Single-view mode / multiple-views mode
        this.setViewMode(!this.multipleViewsCheckBox.checked);
      }
      if (event.code == this.player.keyCodes.userInterface && state) { // Display / hide user interface
        this.setUserInterfaceVisibility(!this.userInterfaceCheckBox.checked);
      }
      if (event.code == this.player.keyCodes.miniMap && state) { // Display / hide mini-map
        this.setMiniMapVisibility(!this.miniMapCheckBox.checked);
      }
      if (event.code == this.player.keyCodes.help && state) { // Display / hide help
        this.setHelpVisibility(!this.helpCheckBox.checked);
      }
      if (event.code == this.player.keyCodes.statistics && state) { // Display / hide statistics
        this.setStatisticsVisibility(!this.statisticsCheckBox.checked);
      }

       */
      if (event.code == this.robot.keyCodes.run) {
        this.robot.keyStates.run = state;
      }
      if (event.code == this.robot.keyCodes.left) {
        this.robot.keyStates.left = state;
      } else if (event.code == this.robot.keyCodes.right) {
        this.robot.keyStates.right = state;
      }
      if (event.code == this.robot.keyCodes.backward) {
        this.robot.keyStates.backward = state;
      } else if (event.code == this.robot.keyCodes.forward) {
        this.robot.keyStates.forward = state;
      }
      if (event.code == this.robot.keyCodes.jump) {
        this.robot.keyStates.jump = state;
      } else if (event.code == this.robot.keyCodes.yes) {
        this.robot.keyStates.yes = state;
      } else if (event.code == this.robot.keyCodes.no) {
        this.robot.keyStates.no = state;
      } else if (event.code == this.robot.keyCodes.wave) {
        this.robot.keyStates.wave = state;
      } else if (event.code == this.robot.keyCodes.punch) {
        this.robot.keyStates.punch = state;
      } else if (event.code == this.robot.keyCodes.thumbsUp) {
        this.robot.keyStates.thumbsUp = state;
      }
    }
  }

  clock!: THREE.Clock;
  animations!: Animations;

  collision(position: THREE.Vector3) {
    return this.floor.distanceToWestWall(position) < this.robot.radius || this.floor.distanceToEastWall(position) < this.robot.radius || this.floor.distanceToNorthWall(position) < this.robot.radius || this.floor.distanceToSouthWall(position) < this.robot.radius;
  }

  update() {
    if (!this.isRunning) {
      if (this.floor.loaded && this.robot.loaded) { // If all resources have been loaded

        // Create the clock
        this.clock = new THREE.Clock();

        // Create model animations (states, emotes and expressions)
        this.animations = new Animations(this.robot.object, this.robot.animations);

        /*
        // Create the user interface
        this.userInterface = new UserInterface(this.scene, this.renderer, this.lights, this.fog, this.player.object, this.animations);

        */

        this.renderer.render(this.scene, this.cameraUsed);

        // Start the game
        this.isRunning = true;
      }
    } else {
      // Update the model animations
      const deltaT = this.clock.getDelta();
      this.animations.update(deltaT);

      // Update the player
      if (!this.animations.actionInProgress) {
        let coveredDistance = this.robot.walkingSpeed * deltaT;
        let directionIncrement = this.robot.turningSpeed * deltaT;
        if (this.robot.keyStates.run) {
          coveredDistance *= this.robot.runningFactor;
          directionIncrement *= this.robot.runningFactor;
        }
        if (this.robot.keyStates.left) {
          this.robot.object.rotateY(directionIncrement);
        } else if (this.robot.keyStates.right) {
          this.robot.object.rotateY(-directionIncrement);
        }
        const direction = this.robot.object.rotation.y;
        if (this.robot.keyStates.backward) {
          const newPosition = new THREE.Vector3(-coveredDistance * Math.sin(direction), 0.0, -coveredDistance * Math.cos(direction)).add(this.robot.object.position);
          if (this.collision(newPosition)) {
            this.animations.fadeToAction("Death", 0.2);
          } else {
            this.animations.fadeToAction(this.robot.keyStates.run ? "Running" : "Walking", 0.2);
            this.robot.object.position.set(newPosition.x, newPosition.y, newPosition.z)
          }
        } else if (this.robot.keyStates.forward) {
          const newPosition = new THREE.Vector3(coveredDistance * Math.sin(direction), 0.0, coveredDistance * Math.cos(direction)).add(this.robot.object.position);
          if (this.collision(newPosition)) {
            this.animations.fadeToAction("Death", 0.2);
          } else {
            this.animations.fadeToAction(this.robot.keyStates.run ? "Running" : "Walking", 0.2);
            this.robot.object.position.set(newPosition.x, newPosition.y, newPosition.z)
          }
        } else if (this.robot.keyStates.jump) {
          this.animations.fadeToAction("Jump", 0.2);
        } else if (this.robot.keyStates.yes) {
          this.animations.fadeToAction("Yes", 0.2);
        } else if (this.robot.keyStates.no) {
          this.animations.fadeToAction("No", 0.2);
        } else if (this.robot.keyStates.wave) {
          this.animations.fadeToAction("Wave", 0.2);
        } else if (this.robot.keyStates.punch) {
          this.animations.fadeToAction("Punch", 0.2);
        } else if (this.robot.keyStates.thumbsUp) {
          this.animations.fadeToAction("ThumbsUp", 0.2);
        } else {
          this.animations.fadeToAction("Idle", this.animations.activeName != "Death" ? 0.2 : 0.6);
        }
      }
    }

    this.renderer.render(this.scene, this.cameraUsed);
  }

  mousePosition!: THREE.Vector2;

  private mouseWheel(event: WheelEvent) {
    // Prevent the mouse wheel from scrolling the document's content
    event.preventDefault();
    // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
    this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
    this.camera.updateZoom(-0.002 * event.deltaY);
  }


  private mouseDown(event: MouseEvent) {
    if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
      // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
      this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
      if (event.buttons == 1) { // Primary button down
        this.changeCameraDistance = true;
      } else { // Secondary button down
        this.changeCameraOrientation = true;
      }
    }
  }

  //}

  private mouseMove(event: MouseEvent) {
    if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
      if (this.changeCameraDistance || this.changeCameraOrientation) { // Mouse action in progress
        // Compute mouse movement and update mouse position
        const newMousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
        const mouseIncrement = newMousePosition.clone().sub(this.mousePosition);
        this.mousePosition = newMousePosition;
        if (event.buttons == 1) { // Primary button down
          if (this.changeCameraDistance) {
            this.camera.updateDistance(-0.05 * (mouseIncrement.x + mouseIncrement.y));
          }
        } else { // Secondary button down
          if (this.changeCameraOrientation) {
            this.camera.updateOrientation(new Orientation(-0.2 * mouseIncrement.x, -0.2 * mouseIncrement.y));
          }
        }
      }
    }
  }

  changeCameraDistance: boolean = false;
  changeCameraOrientation: boolean = false;

  private mouseUp(event: MouseEvent) {
    // Reset mouse move action
    this.changeCameraDistance = false;
    this.changeCameraOrientation = false;
  }

}
