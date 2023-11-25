import * as THREE from "three";
import Ground from "./ground";
import Wall from "./wall";
import Default_data from "./default_data";
import IMapDTO from "../../../dto/IMapDTO";
import Robot from "./robot";

/*
 * parameters = {
 *  url: String,
 *  scale: Vector3
 * }
 */

export default class Floor {
  url: string;
  scale: THREE.Vector3;
  loaded: boolean;
  map: number[][];
  size: { width: number, height: number };
  initialPosition!: THREE.Vector3;
  initialDirection!: number;
  object!: THREE.Group;
  ground!: Ground;
  wall!: Wall;
  robot!: Robot;
  dto!: IMapDTO;
  rooms!: {
    name: string,
    dimensions: { top: { x: number, y: number }, bottom: { x: number, y: number } },
    door: { coordinates: { x: number, y: number }, orientation: string },
  }[];
  passageways!: {
    start: string,
    end: string,
    localization: {
      coordinates: { x: number, y: number },
      orientation: string
    }
  }[];
  elevator!: {
    localization: {
      coordinates: { x: number, y: number },
      orientation: string
    }
  }[];

  constructor(url: string, scale: THREE.Vector3, dto: IMapDTO) {
    this.url = url;
    this.scale = scale;
    this.dto = dto;
    this.rooms = dto.rooms;
    this.passageways = dto.passageways;
    this.elevator = dto.elevator;
    this.map = this.dto.map;
    this.size = {width: this.dto.size.width, height: this.dto.size.length};

    this.loaded = false;

    // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
    THREE.Cache.enabled = true;

    // Load the map
    this.onLoad();
  }

  onLoad() {
    // Store the maze's map and size
    // Store the player's initial position and direction
    this.initialPosition = this.cellToCartesian(Default_data.initialPosition);
    this.initialDirection = Default_data.initialDirection;

    // Create a group of objects
    this.object = new THREE.Group();

    // Create the ground
    this.ground = new Ground(Default_data.groundTextureUrl, new THREE.Vector2(this.size.width, this.size.height));
    this.object.add(this.ground.object);

    // Create a wall
    this.wall = new Wall(Default_data.wallTextureUrl);

    // Build the maze
    let wallObject;
    this.updateMap();
    for (let i = 0; i <= this.size.width; i++) { // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
      for (let j = 0; j <= this.size.height; j++) { // In order to represent the southmost walls, the map height is one row greater than the actual maze height
        /*
         * description.map[][] | North wall | West wall
         * --------------------+------------+-----------
         *          0          |     No     |     No
         *          1          |     No     |    Yes
         *          2          |    Yes     |     No
         *          3          |    Yes     |    Yes
         */
        if (this.map[j][i] == 2 || this.map[j][i] == 3) {
          wallObject = this.wall.object.clone();
          wallObject.position.set(i - this.size.width / 2.0 + 0.5, 0.5, j - this.size.height / 2.0);
          this.object.add(wallObject);
        }
        if (this.map[j][i] == 1 || this.map[j][i] == 3) {
          wallObject = this.wall.object.clone();
          wallObject.rotateY(Math.PI / 2.0);
          wallObject.position.set(i - this.size.width / 2.0, 0.5, j - this.size.height / 2.0 + 0.5);
          this.object.add(wallObject);
        }
      }
    }

    this.loaded = true;
  }

  updateMap() {

    for (let room of this.rooms) {
      // left wall
      for (let i = room.dimensions.top.y; i <= room.dimensions.bottom.y; i++) {
        //checkout if it is a corner of the map - top left
        if (room.dimensions.top.x == 0 && i == 0) {
          this.map[i][room.dimensions.top.x] = 3;
        } else {
          //checkout if it is a corner of the map - bottom left
          if (room.dimensions.top.x == 0 && i == this.size.height) {
            this.map[i][room.dimensions.top.x] = 2;
          } else {
            // checkout if it is a corner of the room - top left
            if (i == room.dimensions.top.y) {
              this.map[i][room.dimensions.top.x] = 3;
            } else {
              // checkout if it is a corner of the room - bottom left
              if (i == room.dimensions.bottom.y) {
                this.map[i][room.dimensions.top.x] = 3;
              } else {
                this.map[i][room.dimensions.top.x] = 1;
              }
            }
          }
        }
      }

      // right wall
      for (let i = room.dimensions.bottom.y; i >= room.dimensions.top.y; i--) {
        console.log(i);
        //checkout if it is a corner of the map - top right
        if (room.dimensions.bottom.x == this.size.width && i == 0) {
          this.map[i][room.dimensions.bottom.x] = 1;
        } else {
          //checkout if it is a corner of the map - bottom right
          if (room.dimensions.bottom.x == this.size.width && i == this.size.height) {
            this.map[i][room.dimensions.bottom.x] = 0;
          } else {
            // checkout if it is a corner of the room - top right
            if (i == room.dimensions.top.y && i == 0) {
              this.map[i][room.dimensions.bottom.x] = 3;
            } else {
              if (i == room.dimensions.bottom.y && i != this.size.height) {
                this.map[i][room.dimensions.bottom.x] = 0;
              } else {
                if (i == room.dimensions.bottom.x && i == this.size.width) {
                  console.log("here");
                  this.map[i][room.dimensions.bottom.x] = 2;
                } else {
                  this.map[i][room.dimensions.bottom.x] = 1;
                }
              }
            }
          }
        }
      }

      /*
      // top wall
      for (let i = room.dimensions.top.x; i <= room.dimensions.bottom.x; i++) {
        //checkout if it is a corner of the map - top left
        if (room.dimensions.top.y == 0 && i == 0) {
          this.map[i][room.dimensions.top.y] = 3;
        } else {
          //checkout if it is a corner of the map - top right
          if (room.dimensions.top.y == this.size.height - 1 && i == this.size.width - 1) {
            this.map[i][room.dimensions.top.y] = 1;
          } else {
            // checkout if it is a corner of the room - top left
            if (i == room.dimensions.top.x) {
              this.map[i][room.dimensions.top.y] = 3;
            } else {
              // checkout if it is a corner of the room - top right
              if (i == room.dimensions.bottom.x) {
                this.map[i][room.dimensions.top.y] = 1;
              } else {
                this.map[i][room.dimensions.top.y] = 1;
              }
            }
          }
        }
      }
      // bottom wall
      for (let i = room.dimensions.bottom.x; i <= room.dimensions.top.x; i--) {
        //checkout if it is a corner of the map - bottom left
        if (room.dimensions.bottom.y == 0 && i == 0) {
          this.map[i][room.dimensions.bottom.y] = 2;
        } else {
          //checkout if it is a corner of the map - bottom right
          if (room.dimensions.bottom.y == this.size.height - 1 && i == this.size.width - 1) {
            this.map[i][room.dimensions.bottom.y] = 0;
          } else {
            // checkout if it is a corner of the room - bottom left
            if (i == room.dimensions.top.x) {
              this.map[i][room.dimensions.bottom.y] = 2;
            } else {
              // checkout if it is a corner of the room - bottom right
              if (i == room.dimensions.bottom.x) {
                this.map[i][room.dimensions.bottom.y] = 0;
              } else {
                this.map[i][room.dimensions.bottom.y] = 1;
              }
            }
          }
        }
      }

       */
      // door
    }
  }

  // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
  cellToCartesian(position: number[]) {
    return new THREE.Vector3((position[1] - this.size.height / 2.0 + 0.5) * this.scale.x, 0.0, (position[0] - this.size.width / 2.0 + 0.5) * this.scale.z)
  }

  // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
  cartesianToCell(position: THREE.Vector3) {
    return [Math.floor(position.z / this.scale.z + this.size.width / 2.0), Math.floor(position.x / this.scale.x + this.size.height / 2.0)];
  }

  distanceToWestWall(position: THREE.Vector3) {
    const indices = this.cartesianToCell(position);
    if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
      return position.x - this.cellToCartesian(indices).x + this.scale.x / 2.0;
    }
    return Infinity;
  }

  distanceToEastWall(position: THREE.Vector3) {
    const indices = this.cartesianToCell(position);
    indices[1]++;
    if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
      return this.cellToCartesian(indices).x - this.scale.x / 2.0 - position.x;
    }
    return Infinity;
  }

  distanceToNorthWall(position: THREE.Vector3) {
    const indices = this.cartesianToCell(position);
    if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
      return position.z - this.cellToCartesian(indices).z + this.scale.z / 2.0;
    }
    return Infinity;
  }

  distanceToSouthWall(position: THREE.Vector3) {
    const indices = this.cartesianToCell(position);
    console.log(indices);
    indices[0]++;
    if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
      return this.cellToCartesian(indices).z - this.scale.z / 2.0 - position.z;
    }
    return Infinity;
  }
}
