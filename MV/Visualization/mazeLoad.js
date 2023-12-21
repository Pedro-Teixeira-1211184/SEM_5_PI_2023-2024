import * as THREE from "three";
import Ground from "./ground.js";
import Wall from "./wall.js";

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
                if (floorInfo.map[j][i] == 2 || floorInfo.map[j][i] == 3) {
                    wallObject = this.wall.object.clone();
                    wallObject.position.set(i - floorInfo.size.width / 2.0 + 0.5, 0.5, j - floorInfo.size.height / 2.0);
                    this.object.add(wallObject);
                }
                if (floorInfo.map[j][i] == 1 || floorInfo.map[j][i] == 3) {
                    wallObject = this.wall.object.clone();
                    wallObject.rotateY(Math.PI / 2.0);
                    wallObject.position.set(i - floorInfo.size.width / 2.0, 0.5, j - floorInfo.size.height / 2.0 + 0.5);
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
}