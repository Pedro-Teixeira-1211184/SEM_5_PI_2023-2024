import {Service, Inject} from 'typedi';
import config from "../../config";
import IRobotService from "./IServices/IRobotService";
import IRobotRepo from "./IRepos/IRobotRepo";
import IRobotDTO from "../dto/IRobotDTO";
import {Result} from "../core/logic/Result";
import {Robot} from "../domain/robot";
import {RobotMapper} from "../mappers/RobotMapper";


@Service()

export default class RobotService implements IRobotService {

    constructor(
        @Inject(config.repos.robot.name) private robotRepo: IRobotRepo
    ) {
    }

    public async createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>> {
        try {
            robotDTO.isActive = true;
            const robotOrError= await Robot.create(robotDTO);
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


}
