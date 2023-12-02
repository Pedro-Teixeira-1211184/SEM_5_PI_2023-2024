import {Mapper} from "../core/infra/Mapper";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {SignUpRequest} from "../domain/signUpRequest";
import {ISignUpRequestDTO} from "../dto/ISignUpRequestDTO";

export class SignUpRequestMap extends Mapper<SignUpRequest> {

  public static toDTO(request: SignUpRequest): ISignUpRequestDTO {
    return {
      //id: user.id.toString(),
      firstName: request.firstName,
      lastName: request.lastName,
      email: request.email,
      password: ""
    } as ISignUpRequestDTO;
  }

  public static async toDomain(raw: any): Promise<SignUpRequest> {
    const userOrError = SignUpRequest.create(
      {
        domainId: raw.domainId,
        firstName: raw.firstName,
        lastName: raw.lastName,
        email: raw.email,
        password: raw.password
      },
      new UniqueEntityID(raw.domainId)
    );

    userOrError.isFailure ? console.log(userOrError.error) : '';

    return userOrError.isSuccess ? userOrError.getValue() : null;
  }

  public static toPersistence(user: SignUpRequest): any {
    return {
      domainId: user.id.toString(),
      email: user.email,
      password: user.password,
      firstName: user.firstName,
      lastName: user.lastName
    };
  }
}
