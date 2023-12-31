using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using DDDSample1.Domain.SystemUser;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.SystemUser
{

    public class UserRepository : BaseRepository<User, UserId>, IUserRepository
    {

    public UserRepository(DDDSample1DbContext context) : base(context.Users)
        {

        }

    }
}