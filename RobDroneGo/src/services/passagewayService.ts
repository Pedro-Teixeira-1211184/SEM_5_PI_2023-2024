import {Service, Inject} from 'typedi';
import config from "../../config";
import IPassagewayDTO from '../dto/IPassagewayDTO';
import {Passageway} from "../domain/passageway";
import IPassagewayRepo from "../services/IRepos/IPassagewayRepo";
import IPassagewayService from "./IServices/IPassagewayService";
import {Result} from "../core/logic/Result";
import {PassagewayMapper} from "../mappers/PassagewayMapper";


@Service()

export default class PassagewayService implements IPassagewayService {

  constructor(
    @Inject(config.repos.passageway.name) private PassagewayRepo: IPassagewayRepo
  ) {
  }

  public async createPassageway(PassagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>> {
    try {

      const PassagewayOrError = await Passageway.create(PassagewayDTO);
      if (PassagewayOrError.isFailure) {
        return Result.fail<IPassagewayDTO>(PassagewayOrError.errorValue());
      }

      const PassagewayResult = PassagewayOrError.getValue();

      //save Passageway
      const PassagewayCreated = await this.PassagewayRepo.save(PassagewayResult);

      if (PassagewayCreated === null) {
        return Result.fail<IPassagewayDTO>('Passageway not created');
      }

      const PassagewayDTOResult = PassagewayMapper.toDTO(PassagewayResult) as IPassagewayDTO;
      return Result.ok<IPassagewayDTO>(PassagewayDTOResult)
    } catch (e) {
      throw e;
    }

  }
}