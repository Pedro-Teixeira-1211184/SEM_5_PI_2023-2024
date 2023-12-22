import * as THREE from "three";
import Ground from "./ground.js";
import Wall from "./wall.js";
import Door from "./door.js";
import Elevator from "./elevator.js";

export default class MazeLoad {
    constructor(floorInfo, initialPosition, initialDirection) {
        // Store the maze's map and size
        this.map = floorInfo.map;
        this.size = floorInfo.size;
        this.scale = new THREE.Vector3(1.0, 1.0, 1.0)

        // Store the player's initial position and direction
        this.initialPosition = this.cellToCartesian(initialPosition);
        this.initialDirection = initialDirection;

        // Create a group of objects
        this.object = new THREE.Group();

        // Create the ground
        this.ground = new Ground({textureUrl: "./textures/ground.jpg", size: floorInfo.size});
        this.object.add(this.ground.object);

        // Create a wall
        this.wall = new Wall({textureUrl: "./textures/wall.jpg"});

        //create a door
        this.door = new Door("./textures/door.jpg", 0xffffff);

        //create a elevator
        this.elevator3D = new Elevator();

        // Create a passageway
        this.bridge = new Door("./textures/bridge.jpg", 0xe26751);

        this.rooms = floorInfo.rooms;
        this.passageways = floorInfo.passageways;
        this.elevator = floorInfo.elevator;

        this.updateMap();

        // Build the maze
        let wallObject;
        for (let i = 0; i <= floorInfo.size.width; i++) { // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
            for (let j = 0; j <= floorInfo.size.height; j++) { // In order to represent the southmost walls, the map height is one row greater than the actual maze height
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
                if (this.map[j][i] == 4) {
                    wallObject = this.door.object.clone();
                    wallObject.position.set(i - this.size.width / 2.0 + 0.5, 0.5, j - this.size.height / 2.0);
                    this.object.add(wallObject);
                }
                if (this.map[j][i] == 5) {
                    wallObject = this.door.object.clone();
                    wallObject.rotateY(Math.PI / 2.0);
                    wallObject.position.set(i - this.size.width / 2.0, 0.5, j - this.size.height / 2.0 + 0.5);
                    this.object.add(wallObject);
                }
                if (this.map[j][i] == 6) {
                    wallObject = this.elevator3D.object.clone();
                    wallObject.position.set(i - this.size.width / 2.0 + 0.5, 0.5, j - this.size.height / 2.0);
                    this.object.add(wallObject);
                }
                if (this.map[j][i] == 7) {
                    wallObject = this.elevator3D.object.clone();
                    wallObject.rotateY(Math.PI / 2.0);
                    wallObject.position.set(i - this.size.width / 2.0, 0.5, j - this.size.height / 2.0 + 0.5);
                    this.object.add(wallObject);
                }
                if (this.map[j][i] == 9) {
                    wallObject = this.bridge.object.clone();
                    wallObject.position.set(i - this.size.width / 2.0 + 0.5, 0.5, j - this.size.height / 2.0);
                    this.object.add(wallObject);
                }
                if (this.map[j][i] == 8) {
                    wallObject = this.bridge.object.clone();
                    wallObject.rotateY(Math.PI / 2.0);
                    wallObject.position.set(i - this.size.width / 2.0, 0.5, j - this.size.height / 2.0 + 0.5);
                    this.object.add(wallObject);
                }
            }
        }

        this.object.scale.set(this.scale.x, this.scale.y, this.scale.z);
        this.loaded = true;

        // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
        THREE.Cache.enabled = true;
    }


    // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
    cellToCartesian(position) {
        return new THREE.Vector3((position[1] - this.size.height / 2.0 + 0.5) * this.scale.x, 0.0, (position[0] - this.size.width / 2.0 + 0.5) * this.scale.z)
    }

    // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
    cartesianToCell(position) {
        return [Math.floor(position.z / this.scale.z + this.size.width / 2.0), Math.floor(position.x / this.scale.x + this.size.height / 2.0)];
    }

    distanceToWestWall(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
            return position.x - this.cellToCartesian(indices).x + this.scale.x / 2.0;
        }
        return Infinity;
    }

    distanceToEastWall(position) {
        const indices = this.cartesianToCell(position);
        indices[1]++;
        if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
            return this.cellToCartesian(indices).x - this.scale.x / 2.0 - position.x;
        }
        return Infinity;
    }

    distanceToNorthWall(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
            return position.z - this.cellToCartesian(indices).z + this.scale.z / 2.0;
        }
        return Infinity;
    }

    distanceToSouthWall(position) {
        const indices = this.cartesianToCell(position);
        indices[0]++;
        if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
            return this.cellToCartesian(indices).z - this.scale.z / 2.0 - position.z;
        }
        return Infinity;
    }

    updateMap() {
        if (this.rooms !== undefined) {
        for (let room of this.rooms) {
            // door
            if (room.door.orientation == "north") {
                this.map[room.door.coordinates.x][room.door.coordinates.y] = 5;
            } else {
                this.map[room.door.coordinates.x][room.door.coordinates.y] = 4;
            }
        }
        }
        if (this.elevator !== undefined) {
        for (let elevator of this.elevator) {
            if (elevator.localization.orientation == "north") {
                this.map[elevator.localization.coordinates.x][elevator.localization.coordinates.y] = 6;
            } else {
                this.map[elevator.localization.coordinates.x][elevator.localization.coordinates.y] = 7;
            }
        }
        }
        // passageways
        if (this.passageways === undefined) return;
        for (let passageway of this.passageways) {
            if (passageway.localization.orientation == "north") {
                this.map[passageway.localization.coordinates.x][passageway.localization.coordinates.y] = 8;
            } else {
                this.map[passageway.localization.coordinates.x][passageway.localization.coordinates.y] = 9;
            }
        }
    }
}