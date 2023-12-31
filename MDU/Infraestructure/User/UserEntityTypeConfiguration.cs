using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.SystemUser;

namespace DDDSample1.Infrastructure.Categories {

    internal class UserEntityTypeConfiguration : IEntityTypeConfiguration<User> {

        public void Configure(EntityTypeBuilder<User> builder) {
            builder.HasKey(b => b.Id);
        }
    }

}