using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Tasks;

namespace DDDSample1.Infrastructure.Tasks
{

    internal class TaskEntityTypeConfiguration : IEntityTypeConfiguration<Task>
    {

        public void Configure(EntityTypeBuilder<Task> builder)
        {
            builder.HasKey(b => b.Id);
        }
    }

}