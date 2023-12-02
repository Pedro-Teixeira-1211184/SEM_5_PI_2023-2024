import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';

export default async ({expressApp}) => {
    const mongoConnection = await mongooseLoader();
    Logger.info('✌️ DB loaded and connected!');

    const userSchema = {
        // compare with the approach followed in repos and services
        name: 'userSchema',
        schema: '../persistence/schemas/userSchema',
    };

    const roleSchema = {
        // compare with the approach followed in repos and services
        name: 'roleSchema',
        schema: '../persistence/schemas/roleSchema',
    };

    const passagewaySchema = {
        name: 'passagewaySchema',
        schema: '../persistence/schemas/passagewaySchema',
    };

    const buildingSchema = {
        name: 'buildingSchema',
        schema: '../persistence/schemas/buildingSchema',
    }

    const floorSchema = {
        name: 'floorSchema',
        schema: '../persistence/schemas/floorSchema',
    }

    const robotSchema = {
        name: 'robotSchema',
        schema: '../persistence/schemas/robotSchema',
    }

    const robotTypeSchema = {
        name: 'robotTypeSchema',
        schema: '../persistence/schemas/robotTypeSchema',
    }

    const roomSchema = {
        name: 'roomSchema',
        schema: '../persistence/schemas/roomSchema',
    }

    const elevatorSchema = {
        name: 'elevatorSchema',
        schema: '../persistence/schemas/elevatorSchema',
    }

    const mapSchema = {
        name: 'mapSchema',
        schema: '../persistence/schemas/mapSchema',
    }

    const taskTypeSchema = {
        name: 'taskTypeSchema',
        schema: '../persistence/schemas/taskTypeSchema',
    }

    const signUpRequestSchema = {
        name: 'signUpRequestSchema',
        schema: '../persistence/schemas/signUpRequestSchema',
    }

    const roleController = {
        name: config.controllers.role.name,
        path: config.controllers.role.path
    }

    const userController = {
        name: config.controllers.user.name,
        path: config.controllers.user.path
    }

    const buildingController = {
        name: config.controllers.building.name,
        path: config.controllers.building.path
    }

    const floorController = {
        name: config.controllers.floor.name,
        path: config.controllers.floor.path
    }

    const robotController = {
        name: config.controllers.robot.name,
        path: config.controllers.robot.path
    }

    const passagewayController = {
        name: config.controllers.passageway.name,
        path: config.controllers.passageway.path
    }

    const roomController = {
        name: config.controllers.room.name,
        path: config.controllers.room.path
    }

    const elevatorController = {
        name: config.controllers.elevator.name,
        path: config.controllers.elevator.path
    }

    const mapController = {
        name: config.controllers.map.name,
        path: config.controllers.map.path
    }

    const taskController = {
        name: config.controllers.task.name,
        path: config.controllers.task.path
    }

    const roleRepo = {
        name: config.repos.role.name,
        path: config.repos.role.path
    }

    const userRepo = {
        name: config.repos.user.name,
        path: config.repos.user.path
    }

    const buildingRepo = {
        name: config.repos.building.name,
        path: config.repos.building.path
    }

    const floorRepo = {
        name: config.repos.floor.name,
        path: config.repos.floor.path
    }

    const robotRepo = {
        name: config.repos.robot.name,
        path: config.repos.robot.path
    }

    const robotTypeRepo = {
        name: config.repos.robotType.name,
        path: config.repos.robotType.path
    }

    const passagewayRepo = {
        name: config.repos.passageway.name,
        path: config.repos.passageway.path
    }

    const roomRepo = {
        name: config.repos.room.name,
        path: config.repos.room.path
    }

    const elevatorRepo = {
        name: config.repos.elevator.name,
        path: config.repos.elevator.path
    }

    const mapRepo = {
        name: config.repos.map.name,
        path: config.repos.map.path
    }

    const taskTypeRepo = {
        name: config.repos.taskType.name,
        path: config.repos.taskType.path
    }

    const signUpRequestRepo = {
        name: config.repos.signUpRequest.name,
        path: config.repos.signUpRequest.path
    }

    const roleService = {
        name: config.services.role.name,
        path: config.services.role.path
    }

    const userService = {
        name: config.services.user.name,
        path: config.services.user.path
    }

    const buildingService = {
        name: config.services.building.name,
        path: config.services.building.path
    }

    const floorService = {
        name: config.services.floor.name,
        path: config.services.floor.path
    }

    const robotService = {
        name: config.services.robot.name,
        path: config.services.robot.path
    }

    const passagewayService = {
        name: config.services.passageway.name,
        path: config.services.passageway.path
    }

    const roomService = {
        name: config.services.room.name,
        path: config.services.room.path

    }

    const elevatorService = {
        name: config.services.elevator.name,
        path: config.services.elevator.path
    }

    const mapService = {
        name: config.services.map.name,
        path: config.services.map.path
    }

    const taskService = {
        name: config.services.task.name,
        path: config.services.task.path
    }

    await dependencyInjectorLoader({
        mongoConnection,
        schemas: [
            roomSchema,
            passagewaySchema,
            userSchema,
            roleSchema,
            buildingSchema,
            floorSchema,
            robotSchema,
            robotTypeSchema,
            elevatorSchema,
            mapSchema,
            taskTypeSchema,
            signUpRequestSchema
        ],
        controllers: [
            roomController,
            passagewayController,
            roleController,
            buildingController,
            floorController,
            robotController,
            elevatorController,
            mapController,
            userController,
            taskController
        ],
        repos: [
            roomRepo,
            passagewayRepo,
            roleRepo,
            userRepo,
            buildingRepo,
            floorRepo,
            robotRepo,
            robotTypeRepo,
            elevatorRepo,
            mapRepo,
            taskTypeRepo,
            signUpRequestRepo
        ],
        services: [
            roomService,
            passagewayService,
            roleService,
            buildingService,
            floorService,
            robotService,
            elevatorService,
            mapService,
            userService,
            taskService
        ]
    });
    Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

    await expressLoader({app: expressApp});
    Logger.info('✌️ Express loaded');
};
