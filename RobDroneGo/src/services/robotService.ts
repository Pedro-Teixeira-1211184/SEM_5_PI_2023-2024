import {Service, Inject} from 'typedi';
import config from "../../config";
import IRobotService from "./IServices/IRobotService";
import IRobotRepo from "./IRepos/IRobotRepo";
import IRobotDTO from "../dto/IRobotDTO";
import {Result} from "../core/logic/Result";
import {Robot} from "../domain/robot";
import {RobotMapper} from "../mappers/RobotMapper";

import IRobotTypeDTO from "../dto/IRobotTypeDTO";
import {RobotTypeMapper} from "../mappers/RobotTypeMapper";
import {RobotType} from "../domain/robotType";
import IRobotTypeRepo from "./IRepos/IRobotTypeRepo";


@Service()

export default class RobotService implements IRobotService {

    constructor(
        @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
        @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo
    ) {
    }

    public async createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>> {
        try {
            //check if robotType exists
            const robotType = await this.robotTypeRepo.findByDesignation(robotDTO.robotType);
            if (robotType === null) {
                return Result.fail<IRobotDTO>('RobotType does not exist');
            }

            if (robotDTO.isActive === undefined) {
                robotDTO.isActive = true;
            }

            const robotOrError = await Robot.create(robotDTO);
            if (robotOrError.isFailure) {
                return Result.fail<IRobotDTO>(robotOrError.errorValue());
            }

            const robotResult = robotOrError.getValue();

            //save robot
            const robotCreated = await this.robotRepo.save(robotResult);

            if (robotCreated === null) {
                return Result.fail<IRobotDTO>('Robot already exists');
            }

            const robotDTOResult = RobotMapper.toDTO(robotCreated);
            return Result.ok<IRobotDTO>(robotDTOResult)

        } catch (e) {
            throw e;
        }
    }

    public async createRobotType(robotTypeDTO: IRobotTypeDTO): Promise<Result<IRobotTypeDTO>> {
        try {
            const robotTypeOrError = await RobotType.create(robotTypeDTO);
            if (robotTypeOrError.isFailure) {
                return Result.fail<IRobotTypeDTO>(robotTypeOrError.errorValue());
            }

            const robotTypeResult = robotTypeOrError.getValue();

            //save robotType
            const robotTypeCreated = await this.robotTypeRepo.save(robotTypeResult);

            if (robotTypeCreated === null) {
                return Result.fail<IRobotTypeDTO>('This type of robot already exists!');
            }

            const robotTypeDTOResult = RobotTypeMapper.toDTO(robotTypeCreated);

            return Result.ok<IRobotTypeDTO>(robotTypeDTOResult)

        } catch (e) {
            throw e;
        }
    }

    public async updateRobot(robot_id: string, body: IRobotDTO): Promise<Result<IRobotDTO>> {
        try {
            const robot = await this.robotRepo.findByDomainId(robot_id);

            if (robot === null) {
                return Result.fail<IRobotDTO>('Robot does not exist');
            }

            if (body.isActive === undefined) {
                robot.isActive = !robot.isActive;
            } else {
                robot.isActive = body.isActive;
            }

            const mapped = RobotMapper.toPersistence(robot)
            const mapped2 = RobotMapper.toDomain(mapped)

            const robotOrError = await this.robotRepo.update(mapped2, robot_id);

            if (robotOrError === null) {
                return Result.fail<IRobotDTO>('Robot already exists');
            }

            const robotDTOResult = RobotMapper.toDTO(robotOrError);

            return Result.ok<IRobotDTO>(robotDTOResult)

        } catch (e) {
            throw e;
        }
    }

}
