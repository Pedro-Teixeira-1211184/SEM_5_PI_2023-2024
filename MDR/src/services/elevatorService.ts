import {Service, Inject} from "typedi";
import config from "../../config";
import {Elevator} from "../domain/elevator";
import IElevatorDTO from "../dto/IElevatorDTO";
import {Result} from "../core/logic/Result";
import IElevatorRepo from "../services/IRepos/IElevatorRepo";
import IElevatorService from "./IServices/IElevatorService";
import {ElevatorMapper} from "../mappers/ElevatorMapper";


@Service()

export default class ElevatorService implements IElevatorService {

    constructor
    (
        @Inject(config.repos.elevator.name) private elevatorRepo: IElevatorRepo
    ) {
    }

    public async createElevator(elevatorDTO: IElevatorDTO): Promise<Result<IElevatorDTO>> {
        try {

            const elevatorOrError = await Elevator.create(elevatorDTO);
            if (elevatorOrError.isFailure) {
                return Result.fail<IElevatorDTO>(elevatorOrError.errorValue());
            }

            const elevatorResult = elevatorOrError.getValue();

            //save elevator
            const elevatorCreated = await this.elevatorRepo.save(elevatorResult);

            if (elevatorCreated === null) {
                return Result.fail<IElevatorDTO>('Elevator not created');
            }

            const elevatorDTOResult = ElevatorMapper.toDTO(elevatorResult) as IElevatorDTO;
            return Result.ok<IElevatorDTO>(elevatorDTOResult)
        } catch (e) {
            throw e;
        }

    }

  public async getElevatorsByBuildingCode(buildingCode: string): Promise<Result<IElevatorDTO[]>> {
    try {
      const elevatorsOrError = await this.elevatorRepo.getElevatorsByBuildingCode(buildingCode);
      if (elevatorsOrError === null) {
        return Result.fail<IElevatorDTO[]>("Elevators not found");
      }

      const elevators = elevatorsOrError as Elevator[];
      const elevatorsDTO: IElevatorDTO[] = [];

      for (let i = 0; i < elevators.length; i++) {
        elevatorsDTO.push(ElevatorMapper.toDTO(elevators[i]) as IElevatorDTO);
      }

      return Result.ok<IElevatorDTO[]>(elevatorsDTO);
    }catch (e) {
      throw e;
    }
  }
}
